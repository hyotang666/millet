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
  (let ((expand?
         (handler-case (system::expand-deftype specifier)
           (program-error ()
             specifier))))
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
                (typecase specifier
                  ((cons
                     (member array base-string bit-vector complex cons
                             double-float float integer long-float mod rational
                             real short-float signed-byte simple-array
                             simple-base-string simple-bit-vector simple-string
                             simple-vector single-float string unsigned-byte
                             vector))
                   (values (ignore-errors
                            (progn (typep '#:dummy specifier) t))))
                  ((cons (eql satisfies) (cons symbol null))
                   (and (fboundp (cadr specifier)) t))
                  ((cons (eql eql) (cons t null)) t)
                  ((cons (eql member)) t)))
               (t nil))))
    (rec (canonicalize-type-specifier type))))
