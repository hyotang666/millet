(in-package :millet)

(defun function-name (function)
  (check-type function function)
  (let ((name (ccl:function-name function)))
    (etypecase name
      ((cons (eql :internal) *) (second name))
      ((cons (eql setf) (cons symbol null)) name)
      (symbol name))))

(defun global-symbol-p (symbol) (ccl:proclaimed-special-p symbol))

(defun lambda-list (arg)
  (multiple-value-bind (result status)
      (ccl:arglist
        (etypecase arg
          (function arg)
          ((cons (eql lambda) t) (coerce arg 'function))
          ((or (and symbol (not keyword)) (cons (eql setf) *)) arg)))
    (if status
        result
        (error "No function defined: ~S" arg))))

(defun type-expand (type)
  (let ((expanded?
         (handler-case (ccl::type-expand type)
           (program-error ()
             type))))
    (if (eq expanded? type)
        (values expanded? nil)
        (values expanded? t))))

(defun type-specifier-p (type)
  (let ((it (ccl:type-specifier-p type)))
    (if it
        (values (ignore-errors (progn (typep '#:dummy type) t)))
        nil)))