; vim: ft=lisp et
(in-package :asdf)
(defsystem :millet
  :version "0.0.3"
  :description "Wrapper for implementation dependent tiny utilities."
  :long-description #.(uiop:read-file-string
                        (uiop:subpathname *load-pathname* "README.md"))
  :author "Shinichi Sato"
  :license "MIT"
  :components((:file "millet")))

;; Push form below is added by JINGOH.GENERATOR.
(defmethod component-depends-on((o test-op) (c (eql (find-system "millet"))))
  (append (call-next-method)'((test-op "millet.test"))))
(defmethod operate :around(o (c (eql (find-system "millet")))
                             &key ((:compile-print *compile-print*))
                             ((:compile-verbose *compile-verbose*))
                             &allow-other-keys)
  (call-next-method))
