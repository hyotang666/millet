(in-package :millet)

(defun function-name (function)
  (check-type function function)
  (let ((name? (ext:compiled-function-name function)))
    (typecase name? ((cons (eql lambda)) nil) (t name?))))

(defun global-symbol-p (symbol)
  (check-type symbol symbol)
  (or (constantp symbol) (ext:specialp symbol)))

(defun lambda-list (arg)
  (typecase arg
    ((cons (eql lambda)) (second arg))
    (otherwise (ext:function-lambda-list (coerce arg 'function)))))

(defun type-expand (type)
  (let ((expander (ext:type-expander type)))
    (if expander
        (values (funcall expander nil type) t)
        (values type nil))))

(eval-when (:load-toplevel) (incomplete 'type-specifier-p))