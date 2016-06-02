(in-package :cl-user)
(defpackage :millet(:use :cl)
  (:export
    #:function-name
    #:lambda-list
    #:global-symbol-p
    ))
(in-package :millet)

(eval-when(:compile-toplevel :load-toplevel)
  (defun doc(system namestring)
    (uiop:read-file-string
      (uiop:subpathname(asdf:system-source-directory(asdf:find-system system))
	namestring))))

; FUNCTION-LAMBDA-EXPRESSION sometimes fail to return function name.
; especially when hard optimization is specified.
(defun function-name(function)
  #.(doc :millet "doc/function-name.F.md")
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
      #+sbcl `(let((it(sb-pcl::fun-name function)))
	       (unless(typep it '(cons (eql lambda)t))
		 it))
      `(nth-value 2(function-lambda-expression function)))) ; as default.

(defun global-symbol-p(symbol)
  #.(doc :millet "doc/global-symbol-p.F.md")
  (check-type symbol symbol)
  #.(or ; to avoid write #-(or ...) ; be care to quote!
      #+clisp `(ext:special-variable-p symbol)
      #+ccl `(ccl:proclaimed-special-p symbol)
      #+sbcl `(or (constantp symbol)
		  (sb-walker:var-globally-special-p symbol))
      #+ecl `(or (constantp symbol)
		 (walker:variable-special-p symbol nil))
     `(not-support-warning 'global-symbol-p))) ; as default.

(defun lambda-list(arg)
  #.(doc :millet "doc/lambda-list.F.md")
  #.(or ; to avoid write #-(or ...)
      #+clisp `(ext:arglist arg)
      #+ccl `(ccl:arglist (or(and(typep arg '(cons (eql lambda) T))
			       (coerce arg 'function))
			    arg))
      #+ecl `(ext:function-lambda-list(or(and(typep arg '(cons (eql lambda)T))
					   (coerce arg 'function))
					arg))
      #+sbcl `(sb-kernel:%fun-lambda-list(or(and(symbolp arg)
					      (macro-function arg))
					   (coerce arg 'function)))
      `(not-support-warning 'lambda-list))) ; as default.

(defun not-support-warning (api)
  (warn "~S is not implemented in ~A" api uiop:*implementation-type*))
