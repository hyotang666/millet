; vim: ft=lisp et
(in-package :asdf)
(defsystem :millet
  :depends-on (:closer-mop)
  :in-order-to ((test-op (test-op :millet-test)))
  :components((:file "millet")))

(defsystem :millet-test
  :depends-on (:jingoh :millet)
  :components ((:file "design"))
  :perform (test-op(o s)
             (uiop:symbol-call :jingoh 'report)))

