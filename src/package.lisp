(in-package :cl-user)

(defpackage :millet
  (:use :cl)
  (:export #:function-name
           #:lambda-list
           #:global-symbol-p
           #:special-symbol-p
           #:type-expand
           #:type-specifier-p))

(in-package :millet)

(defun incomplete (api)
  (warn "~S is incomplete in ~S." api (lisp-implementation-type)))

(defun not-support (api)
  (error "~S is not implemented in ~A" api (lisp-implementation-type)))