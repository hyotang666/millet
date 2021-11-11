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

(defun canonicalize-type-specifier (specifier)
  ;; [clhs says](http://clhs.lisp.se/Body/04_bc.htm)
  ;; > If a list has one or more unspecified items at the end, those items can be dropped.
  ;; > If dropping all occurrences of * results in a singleton list,
  ;; > then the parentheses can be dropped as well (the list can be replaced by the symbol in its car).
  ;; > For example, (vector double-float *) can be abbreviated to (vector double-float),
  ;; > and (vector * *) can be abbreviated to (vector) and then to vector.
  (if (and (consp specifier) (null (cdr specifier))) ; singleton.
      (car specifier)
      specifier))