(in-package :cl-user)
(defpackage :millet(:use :cl)
  (:export
    #:function-name
    #:lambda-list
    #:global-symbol-p
    #:special-symbol-p
    #:type-expand
    #:type-specifier-p
    ))
(in-package :millet)

; FUNCTION-LAMBDA-EXPRESSION sometimes fail to return function name.
; especially when hard optimization is specified.
(defun function-name(function)
  (check-type function function)
  #.(or ; to avoid write #-(or ...) ; be care to quote!
      #+clisp `(let((it(sys::function-name function)))
		 (etypecase it
		   (symbol (if(eq :lambda it)
			     nil
			     (and (symbol-package it) it))); when compile time, sys::function-name return uninterned symbol which bound by lambda function.
		   ((cons(eql setf)t) it))) ; for (setf foo)
      #+ecl `(if(eq(type-of function) 'standard-generic-function)
	      (clos:generic-function-name function)
	      (ext:compiled-function-name function))
      #+ccl `(ccl:function-name function)
      #+sbcl `(let((it(sb-kernel::%fun-name function)))
	       (unless(typep it '(cons (eql lambda)t))
		 it))
      `(nth-value 2(function-lambda-expression function)))) ; as default.

(defun global-symbol-p(symbol)
  (check-type symbol symbol)
  #.(or ; to avoid write #-(or ...) ; be care to quote!
      #+clisp `(ext:special-variable-p symbol)
      #+ccl `(ccl:proclaimed-special-p symbol)
      #+sbcl `(or (constantp symbol)
		  (sb-walker:var-globally-special-p symbol))
      #+ecl `(or (constantp symbol)
		 (walker:variable-special-p symbol nil))
     `(not-support-warning 'global-symbol-p))) ; as default.

(defun special-symbol-p(symbol)
  (and (global-symbol-p symbol)
       (not(constantp symbol))))

(defun lambda-list(arg)
  #.(or ; to avoid write #-(or ...)
      #+clisp `(ext:arglist arg)
      #+ccl `(ccl:arglist (etypecase arg
			    (FUNCTION arg)
			    ((CONS (EQL LAMBDA)T)
			     (coerce arg 'function))
			    ((AND SYMBOL (NOT KEYWORD))
			     arg)))
      #+ecl `(ext:function-lambda-list(or(and(typep arg '(cons (eql lambda)T))
					   (coerce arg 'function))
					arg))
      #+sbcl `(sb-kernel:%fun-lambda-list(or(and(symbolp arg)
					      (macro-function arg))
					   (coerce arg 'function)))
      `(not-support-warning 'lambda-list))) ; as default.

(defun not-support-warning (api)
  (warn "~S is not implemented in ~A" api uiop:*implementation-type*))

(defun type-expand(type)
  #.(or
      #+clisp `(handler-case(ext:type-expand type)
		 (error()(values type nil)))
      #+sbcl `(multiple-value-bind(result condition)(ignore-errors(sb-ext:typexpand type))
		(if (or (null condition)
			(typep condition 'condition))
		  (values type nil)
		  (values result T)))
      #+ccl `(let((expanded?(ccl::type-expand type)))
	       (if (eq expanded? type)
		 (values expanded? NIL)
		 (values expanded? T)))
      #+ecl `(let((expanded?(si::expand-deftype type)))
	       (if(eq expanded? type)
		 (values expanded? NIL)
		 (values expanded? T)))
      `(not-support-warning 'type-expand)))

(defun type-specifier-p(type)
  #.(or
      #+sbcl `(sb-ext:valid-type-specifier-p type)
      #+ccl `(let((it(ccl:type-specifier-p type)))
	       (if it T NIL))
      `(labels((rec(specifier)
		 (if(typep specifier '(OR (CONS (EQL AND)T)
					  (CONS (EQL OR)T)))
		    (every #'rec (cdr specifier))
		    (values(ignore-errors(progn (typep '#:dummy specifier)
						T))))))
	 (rec type))))
