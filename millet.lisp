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

(defun incomplete (api)
  (warn "~S is incomplete in ~S." api uiop:*implementation-type*))

;;; FUNCTION-LAMBDA-EXPRESSION sometimes fail to return function name.
;;; especially when hard optimization is specified.

(defun function-name (function)
  (check-type function function)
  #.(or ; to avoid write #-(or ...) be care to quote!
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

(defun not-support (api)
  (error "~S is not implemented in ~A" api uiop:*implementation-type*))

(define-compiler-macro not-support (&whole whole api &environment env)
  (when (constantp api env)
    (warn "~S is not implemented in ~A" (eval api) uiop:*implementation-type*))
  whole)

(defun global-symbol-p (symbol)
  (check-type symbol symbol)
  #.(or ; to avoid write #-(or ...) be care to quote!
        #+clisp
        `(ext:special-variable-p symbol)
        #+ccl
        `(ccl:proclaimed-special-p symbol)
        #+sbcl
        `(or (constantp symbol) (sb-walker:var-globally-special-p symbol))
        #+ecl
        `(or (constantp symbol) (walker:variable-special-p symbol nil))
        ;; as default.
        `(progn
          symbol ; to muffle unused warning.
          (not-support 'global-symbol-p))))

(defun special-symbol-p (symbol)
  (and (global-symbol-p symbol) (not (constantp symbol))))

(defun lambda-list (arg)
  #.(or ; to avoid write #-(or ...)
        #+clisp
        `(ext:arglist arg)
        #+ccl
        `(multiple-value-bind (result status)
             (ccl:arglist
               (etypecase arg
                 (function arg)
                 ((cons (eql lambda) t) (coerce arg 'function))
                 ((or (and symbol (not keyword)) (cons (eql setf) *)) arg)))
           (if status
               result
               (error "No function defined: ~S" arg)))
        #+ecl
        `(ext:function-lambda-list
           (typecase arg
             ((cons (eql lambda) *) (coerce arg 'function))
             ((cons (eql setf) *) (fdefinition arg))
             (t arg)))
        #+sbcl
        `(let ((function
                (or (and (symbolp arg) (macro-function arg))
                    (coerce arg 'function))))
           (if (typep function 'standard-generic-function)
               (sb-mop:generic-function-lambda-list function)
               (sb-kernel:%fun-lambda-list function)))
        #+lispworks
        `(lw:function-lambda-list arg)
        ;; as default.
        `(progn
          arg ; to muffle unused warning.
          (not-support 'lambda-list))))

(defun type-expand (type)
  #.(or #+clisp
        `(handler-case (ext:type-expand type)
           (error ()
             (values type nil)))
        #+sbcl
        `(handler-case (sb-ext:typexpand type)
           (error ()
             (values type nil)))
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
        #+lispworks
        `(type:expand-user-type type)
        ;; as default.
        `(progn
          type ; to muffle unused warning.
          (not-support 'type-expand))))

#+ecl
(eval-when (:load-toplevel) (incomplete 'type-specifier-p))

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
        #+lispworks
        `(type:valid-type-specifier type)
        ;; as default.
        `(labels ((rec (specifier)
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
