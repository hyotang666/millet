(in-package :millet)

(eval-when (:compile-toplevel :load-toplevel)
  (defun already-defined-p (symbol)
    (if (fboundp symbol)
        '(and)
        '(or))))

#+#.(millet::already-defined-p 'function-name)
(defun function-name (function)
  (check-type function function)
  (nth-value 2 (function-lambda-expression function)))

#+#.(millet::already-defined-p 'global-symbol-p)
(defun global-symbol-p (symbol)
  (declare (ignore symbol))
  (not-support 'global-symbol-p))

#+#.(millet::already-defined-p 'special-symbol-p)
(defun special-symbol-p (symbol)
  (and (global-symbol-p symbol) (not (constantp symbol))))

#+#.(millet::already-defined-p 'lambda-list)
(defun lambda-list (arg) (declare (ignore arg)) (not-support 'lambda-list))

#+#.(millet::already-defined-p 'type-expand)
(defun type-expand (type) (declare (ignore type)) (not-support 'type-expand))

#+#.(millet::already-defined-p 'type-specifier-p)
(defun type-specifier-p (type)
  (labels ((rec (specifier)
             (cond
               ((typep specifier '(cons (member and or) *))
                (every #'rec (cdr specifier)))
               ;; At least ECL needs.
               ((or (symbolp specifier)
                    (consp specifier)
                    (typep specifier 'standard-class))
                (values (ignore-errors (progn (typep '#:dummy specifier) t))))
               (t nil))))
    (rec type)))