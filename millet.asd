; vim: ft=lisp et
(in-package :asdf)
(defsystem :millet
  :description "Tiny utilities which abandoned by alexandria."
  :long-description #.(uiop:read-file-string
                        (uiop:subpathname *load-pathname* "README.md"))
  :author "Shinichi Sato"
  :license "MIT"
  :components((:file "millet")))

;; Push form below is added by JINGOH.GENERATOR.
(defmethod component-depends-on((o test-op) (c (eql (find-system "millet"))))
  (append (call-next-method)'((test-op "millet.test"))))
