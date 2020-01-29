(in-package :cl-user)

(defpackage :millet
  (:use :cl)
  (:export #:function-name
           #:lambda-list
           #:global-symbol-p
           #:special-symbol-p
           #:type-expand
           #:type-specifier-p))

(in-package :millet)

;;; FUNCTION-LAMBDA-EXPRESSION sometimes fail to return function name.
;;; especially when hard optimization is specified.

(defun function-name (function)
  (check-type function function)
  #.(or ; to avoid write #-(or ...) ; be care to quote!
        #+clisp
        `(let ((it (sys::function-name function)))
           (etypecase it
             (symbol
              (if (eq :lambda it)
                  nil
                  (and (symbol-package it) it))) ; when compile time,
                                                 ; sys::function-name return
                                                 ; uninterned symbol which
                                                 ; bound by lambda function.
             ((cons (eql setf) t) it))) ; for (setf foo)
        #+ecl
        `(if (eq (type-of function) 'standard-generic-function)
             (clos:generic-function-name function)
             (ext:compiled-function-name function))
        #+ccl
        `(let ((name (ccl:function-name function)))
           (etypecase name
             ((cons (eql :internal) *) (second name))
             ((cons (eql setf) (cons symbol null)) name)
             (symbol name)))
        #+sbcl
        `(let ((it (sb-kernel:%fun-name function)))
           (typecase it
             ((cons (eql lambda) t) nil)
             ((cons (eql setf) t) it)
             (list (cadr it))
             (string nil) ; formatter
             (t it)))
        ;; as default.
        `(nth-value 2 (function-lambda-expression function))))

(defun global-symbol-p (symbol)
  (check-type symbol symbol)
  #.(or ; to avoid write #-(or ...) ; be care to quote!
        #+clisp
        `(ext:special-variable-p symbol)
        #+ccl
        `(ccl:proclaimed-special-p symbol)
        #+sbcl
        `(or (constantp symbol) (sb-walker:var-globally-special-p symbol))
        #+ecl
        `(or (constantp symbol) (walker:variable-special-p symbol nil))
        ;; as default.
        `(not-support-warning 'global-symbol-p)))

(defun special-symbol-p (symbol)
  (and (global-symbol-p symbol) (not (constantp symbol))))

(defun lambda-list (arg)
  #.(or ; to avoid write #-(or ...)
        #+clisp
        `(ext:arglist arg)
        #+ccl
        `(ccl:arglist
           (etypecase arg
             (function arg)
             ((cons (eql lambda) t) (coerce arg 'function))
             ((and symbol (not keyword)) arg)))
        #+ecl
        `(ext:function-lambda-list
           (or (and (typep arg '(cons (eql lambda) t)) (coerce arg 'function))
               arg))
        #+sbcl
        `(let ((function
                (or (and (symbolp arg) (macro-function arg))
                    (coerce arg 'function))))
           (if (typep function 'standard-generic-function)
               (sb-mop:generic-function-lambda-list function)
               (sb-kernel:%fun-lambda-list function)))
        ;; as default.
        `(not-support-warning 'lambda-list)))

(defun not-support-warning (api)
  (warn "~S is not implemented in ~A" api uiop/os:*implementation-type*))

(defun type-expand (type)
  #.(or #+clisp
        `(handler-case (ext:type-expand type)
           (error ()
             (values type nil)))
        #+sbcl
        `(multiple-value-bind (result condition)
             (ignore-errors (sb-ext:typexpand type))
           (if (or (null condition) (typep condition 'condition))
               (values type nil)
               (values result t)))
        #+ccl
        `(let ((expanded? (ccl::type-expand type)))
           (if (eq expanded? type)
               (values expanded? nil)
               (values expanded? t)))
        #+ecl
        `(let ((expanded? (si::expand-deftype type)))
           (if (eq expanded? type)
               (values expanded? nil)
               (values expanded? t)))
        `(not-support-warning 'type-expand)))

(defun type-specifier-p (type)
  #.(or #+sbcl
        `(handler-case (sb-ext:valid-type-specifier-p type)
           (sb-kernel:parse-unknown-type ()
             nil))
        #+ccl
        `(let ((it (ccl:type-specifier-p type)))
           (if it
               (values (ignore-errors (progn (typep '#:dummy type) t)))
               nil))
        `(labels ((rec (specifier)
                    (if (typep specifier
                               '(or (cons (eql and) t) (cons (eql or) t)))
                        (every #'rec (cdr specifier))
                        (values
                          (ignore-errors (progn (typep '#:dummy specifier) t))))))
           (rec type))))