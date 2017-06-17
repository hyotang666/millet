; vim: ft=lisp et
(in-package :asdf)
(defsystem :millet
  :description "Tiny utilities which abandoned by alexandria."
  :long-description #.(uiop:read-file-string
                        (uiop:subpathname *load-pathname* "README.md"))
  :author "Shinichi Sato"
  :components((:file "millet")))
;; Perform method below is added by JINGOH.GENERATOR.
(defmethod perform ((o test-op) (c (eql (find-system "millet"))))
 (test-system :millet.test))
