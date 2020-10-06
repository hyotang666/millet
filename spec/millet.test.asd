; vim: ft=lisp et
(in-package :asdf)
(defsystem :millet.test
  :version "1.0.7"
  :depends-on (:jingoh "millet") :components
 ((:file "millet")) :perform (test-op (o c) (symbol-call :jingoh :examine :millet)))
