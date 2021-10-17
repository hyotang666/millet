; vim: ft=lisp et
(in-package :asdf)
(defsystem :millet
  :version "0.4.6"
  :description "Wrapper for implementation dependent tiny utilities."
  :long-description #.(uiop:read-file-string
                        (uiop:subpathname *load-pathname* "README.md"))
  :source-control (:git "git@github.com:hyotang666/millet")
  :bug-tracker "https://github.com/hyotang666/millet/issues"
  :author "SATO Shinichi"
  :license "MIT"
  :serial t
  :pathname "src"
  :components((:file "package")
              (:file "sbcl" :if-feature :sbcl)
              (:file "clisp" :if-feature :clisp)
              (:file "ecl" :if-feature :ecl)
              (:file "ccl" :if-feature :ccl)
              (:file "allegro" :if-feature :allegro)
              (:file "clasp" :if-feature :clasp)
              (:file "lispworks" :if-feature :lispworks)
              (:file "cmu" :if-feature :cmu)
              (:file "abcl" :if-feature :abcl)
              (:file "default")))

;;; These forms below are added by JINGOH.GENERATOR.
;; Ensure in ASDF for pretty printings.
(in-package :asdf)
;; Enable testing via (asdf:test-system "millet").
(defmethod component-depends-on ((o test-op) (c (eql (find-system "millet"))))
  (append (call-next-method) '((test-op "millet.test"))))
;; Enable passing parameter for JINGOH:EXAMINER via ASDF:TEST-SYSTEM.
(defmethod operate :around
           ((o test-op) (c (eql (find-system "millet")))
            &rest keys
            &key ((:compile-print *compile-print*))
            ((:compile-verbose *compile-verbose*)) &allow-other-keys)
  (flet ((jingoh.args (keys)
           (loop :for (key value) :on keys :by #'cddr
                 :when (find key '(:on-fails :subject :vivid) :test #'eq)
                 :collect key
                 :and
                 :collect value :else
                 :when (eq :jingoh.verbose key)
                 :collect :verbose
                 :and
                 :collect value)))
    (let ((args (jingoh.args keys)))
      (declare (special args))
      (call-next-method))))
;; Enable importing spec documentations.
(let ((system (find-system "jingoh.documentizer" nil)))
  (when (and system (not (featurep :clisp)))
    (load-system system)
    (defmethod perform :after ((o load-op) (c (eql (find-system "millet"))))
      (with-muffled-conditions (*uninteresting-conditions*)
        (handler-case (symbol-call :jingoh.documentizer :import c)
                      (error (condition)
                             (warn "fails to import documentation of ~s.~%~a"
                                   (coerce-name c)
                                   (princ-to-string condition))))))))
