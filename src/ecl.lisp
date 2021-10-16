(in-package :millet)

(defun function-name (function)
  (if (eq (type-of function) 'standard-generic-function)
      (clos:generic-function-name function)
      (ext:compiled-function-name function)))

(defun global-symbol-p (symbol)
  (check-type symbol symbol)
  (or (constantp symbol) (walker:variable-special-p symbol nil)))

(defun lambda-list (arg)
  (ext:function-lambda-list
    (typecase arg
      ((cons (eql lambda) *) (coerce arg 'function))
      ((cons (eql setf) *) (fdefinition arg))
      (t arg))))

(defun type-expand (type)
  (let ((expanded? (si::expand-deftype type)))
    (if (eq expanded? type)
        (values expanded? nil)
        (values expanded? t))))

(eval-when (:load-toplevel) (incomplete 'type-specifier-p))