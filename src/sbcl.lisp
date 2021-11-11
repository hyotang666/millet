(in-package :millet)

(defun function-name (function)
  (let ((it (sb-kernel:%fun-name function)))
    (typecase it
      ((cons (eql lambda) t) nil)
      ((cons (eql setf) t) it)
      (list (cadr it))
      (string nil) ; formatter
      (t it))))

(defun global-symbol-p (symbol)
  (check-type symbol symbol)
  (or (constantp symbol) (sb-walker:var-globally-special-p symbol)))

(defun lambda-list (arg)
  (let ((function
         (or (and (symbolp arg) (macro-function arg)) (coerce arg 'function))))
    (if (typep function 'standard-generic-function)
        (sb-mop:generic-function-lambda-list function)
        (sb-kernel:%fun-lambda-list function))))

(defun type-expand (type)
  (handler-case (sb-ext:typexpand type)
    (error ()
      (values type nil))))

(defun type-specifier-p (type)
  (handler-case
      (sb-ext:valid-type-specifier-p (canonicalize-type-specifier type))
    (sb-kernel:parse-unknown-type ()
      nil)))