; vim: ft=lisp et
(in-package :asdf)
(defsystem :millet.test
  :version "1.0.22"
  :depends-on
  (:jingoh
    "millet"
    "closer-mop" ; Wrapper for meta-object-protocols.
    )
  :components
 ((:file "millet")) :perform (test-op (o c) (symbol-call :jingoh :examine :millet)))
