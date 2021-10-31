(in-package :millet)

(defun function-name (function)
  (let ((name? (nth-value 2 (function-lambda-expression function))))
    (typecase name? ((cons (eql :internal)) nil) (otherwise name?))))

(defun global-symbol-p (symbol)
  (check-type symbol symbol)
  (or (constantp symbol) (excl::variable-special-p symbol nil)))

(defun lambda-list (arg)
  (typecase arg
    ((cons (eql lambda)) (second arg))
    (otherwise (excl:arglist arg))))

(defun type-expand (type)
  (let ((expand?
         (handler-case (excl::deftype-expand type)
           (program-error ()
             type))))
    (if (eq expand? type)
        (values expand? nil)
        (values expand? t))))