(in-package :millet)

(unless (fboundp 'function-name)
  (defun function-name (function)
    (check-type function function)
    (nth-value 2 (function-lambda-expression function))))

(unless (fboundp 'global-symbol-p)
  (defun global-symbol-p (symbol)
    (declare (ignore symbol))
    (not-support 'global-symbol-p)))

(unless (fboundp 'special-symbol-p)
  (defun special-symbol-p (symbol)
    (and (global-symbol-p symbol) (not (constantp symbol)))))

(unless (fboundp 'lambda-list)
  (defun lambda-list (arg) (declare (ignore arg)) (not-support 'lambda-list)))

(unless (fboundp 'type-expand)
  (defun type-expand (type) (declare (ignore type)) (not-support 'type-expand)))

(unless (fboundp 'type-specifier-p)
  (defun type-specifier-p (type)
    (labels ((rec (specifier)
               (cond
                 ((typep specifier '(cons (member and or) *))
                  (every #'rec (cdr specifier)))
                 ;; At least ECL needs.
                 ((or (symbolp specifier)
                      (consp specifier)
                      (typep specifier 'standard-class))
                  (values (ignore-errors
                           (progn (typep '#:dummy specifier) t))))
                 (t nil))))
      (rec type))))