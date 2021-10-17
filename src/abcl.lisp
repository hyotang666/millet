(in-package :millet)

;; :key

(defun lambda-list (arg)
  (typecase arg
    (standard-generic-function (mop:generic-function-lambda-list arg))
    (function (extensions:arglist arg))
    ((cons (eql lambda)) (second arg))
    ((cons (eql setf)) (lambda-list (fdefinition arg)))
    (symbol (lambda-list (fdefinition arg)))))

(defun global-symbol-p (symbol)
  (check-type symbol symbol)
  (ext:special-variable-p symbol))

(defun special-symbol-p (symbol)
  (and (global-symbol-p symbol) (not (constantp symbol))))

(defun type-expand (specifier)
  (let ((expand? (system::expand-deftype specifier)))
    (if (eq expand? specifier)
        (values specifier nil)
        (values expand? t))))

(defun type-specifier-p (type)
  (labels ((rec (specifier)
             (cond
               ((typep specifier '(cons (member and or not)))
                (every #'rec (cdr specifier)))
	       ((and (symbolp specifier)
		     (not (keywordp specifier)))
		(system::known-type-p specifier))
	       ((typep specifier 'standard-class) t)
               ((consp specifier)
                (values (ignore-errors (progn (typep '#:dummy specifier) t))))
               (t nil))))
    (rec type)))

(eval-when (:load-toplevel) (incomplete 'type-specifier-p))
