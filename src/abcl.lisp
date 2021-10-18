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
               ((member specifier '(t nil structure-object)) t)
               ((and (symbolp specifier)
                     (let ((spec (get specifier 'system::source)))
                       (and spec
                            (or (assoc :type spec)
                                (assoc :structure spec)
                                (assoc :class spec)))))
                t)
               ((and (symbolp specifier) (not (keywordp specifier)))
                (multiple-value-bind (spec expand?)
                    (type-expand specifier)
                  (if expand?
                      (rec spec)
                      (system::known-type-p specifier))))
               ((typep specifier 'standard-class) t)
               ((consp specifier)
                (values (ignore-errors (progn (typep '#:dummy specifier) t))))
               (t nil))))
    (rec type)))

(eval-when (:load-toplevel) (incomplete 'type-specifier-p))