(defpackage :millet.spec (:use :cl :jingoh :millet))
(in-package :millet.spec)
(setup :millet)

(requirements-about FUNCTION-NAME :doc-type function)

;;;; Description:
; accept function, return its name.
#?(function-name #'car) => CAR

#+syntax
(FUNCTION-NAME function) ; => result

;;;; Arguments and Values:

; function := function, otherwise error.
#?(function-name 'car) :signals error

; result := symbol

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:
; When lambda function comes, nil is returned.
#?(function-name #'(lambda () (print :hoge)))
=> NIL

; When local function comes, unspecified.
#?(function-name (flet ((test () :hoge)) #'test)) => unspecified
#?(function-name (labels ((test () :hoge)) #'test)) => unspecified
; ECL needs above spec.
; Without compile, it works.
#+ecl
#?(function-name (flet ((test () :hoge)) #'test)) => TEST
; But with compile.
#+ecl
#?(function-name (compile nil (flet ((test () :hoge)) #'test))) => NIL ; not works.
; ECL specific two tests above are as observer.
; When failed, MILLET needs to change spec or impl.

#| Comment out since above ECL specific needs.
; CCL specific issue. (macro) returns (:INTERNAL HOGE MACRO).
#?(defmacro macro ()
    (flet ((hoge ()))
      `',(function-name #'hoge)))
=> MACRO
,:before (fmakunbound 'macro)

#?(macro)
=> HOGE
|#

; SBCL specific issue. FORMATTER returns e.g. "fmt$~A".
; When FORMATTER comes, return NIL.
#?(function-name (formatter "~A")) => NIL

; When setf function comes, (SETF NAME) is returned.
#?(defclass foo () ((bar :accessor foo-bar)))
:satisfies (lambda ($arg)
	     (& (typep $arg 'standard-class)
		(eq 'foo (class-name $arg))))
,:before (progn (mapc #'fmakunbound '(foo-bar (setf foo-bar)))
		(unintern 'foo))

#?(function-name (fdefinition '(setf foo-bar)))
=> (SETF FOO-BAR)
,:test equal

;;;; Exceptional-Situations:

(requirements-about LAMBDA-LIST :doc-type function)

;;;; Description:
; accept function, return its lambda-list
#?(lambda-list #'lambda-list)
=> (MILLET::ARG)
,:test equal

#+syntax
(LAMBDA-LIST arg) ; => result

;;;; Arguments and Values:

; arg := function designator, otherwise error
#?(lambda-list :not-function-designator) :signals error

; result := lambda-list

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:
; function is not defined, an error is signaled.
#?(lambda-list 'no-such-function) :signals error

;;;; Tests.
; Works with generic-function.
#?(lambda-list 'documentation)
:satisfies (lambda (result)
             (& (listp result)
                (= 2 (length result))))

; Works with macros.
#?(defmacro test (a) `,a)
=> TEST
,:before (fmakunbound 'test)

#?(lambda-list 'test)
=> (A)
,:test #'equal

; Not support CL macros due to clisp can not.
#?(lambda-list 'when) => unspecified

; CLISP specific guard.
#+clisp
#?(typep (lambda-list (macro-function 'when))
         '(cons symbol
                (cons (member &body &rest)
                      (cons symbol null))))
=> NIL

; Works with lambda.
#?(lambda-list (lambda (a) a))
=> (A)
,:test #'equal

; Works with lambda-form.
#?(lambda-list '(lambda (a) a))
=> (A)
,:test #'equal

; Not supported local functions due to ECL specific issue.
#?(lambda-list (flet ((test (a) a)) #'test)) => unspecified
; ECL specific guard.
#+ecl
#?(lambda-list (compile nil (flet ((test (a) a)) #'test)))
=> NIL

; Works with '(setf fun-name).
#?(lambda-list '(setf foo-bar))
:satisfies (lambda (result)
             (typep result '(cons symbol (cons symbol null))))

; Works with formatter.
#?(lambda-list (formatter "~A"))
:satisfies (lambda (result)
             (typep result '(cons symbol *)))

; Not works with complement.
#?(lambda-list (complement #'car))
=> unspecified
; Due to in SBCL, lambda list will change after compile.
; Before compile (&REST SB-IMPLE::ARGUMENTS)
; After compile (#:G1).
#+sbcl
#?(lambda-list (complement #'car))
:satisfies (lambda (result)
             (typep result '(cons (eql &rest) (cons symbol null))))
,:lazy t
#+sbcl
#?(lambda-list (complement #'car))
:satisfies (lambda (result)
             (typep result '(cons symbol null)))
#+ecl
#?(lambda-list (complement #'car))
=> NIL

; Not works with constantly due to ccl could not.
#?(lambda-list (constantly nil))
=> unspecified

#+ccl ; Guard for ccl.
#?(lambda-list (constantly nil))
=> NIL

; Not works with funcallable-standard-class
#?(defclass fun ()
    ()
    (:metaclass c2mop:funcallable-standard-class))
:be-the c2mop:funcallable-standard-class

#?(let ((fun (make-instance 'fun)))
    (c2mop:set-funcallable-instance-function fun #'car)
    (lambda-list fun))
=> unspecified

#+ccl ; Guard for ccl.
#?(let ((fun (make-instance 'fun)))
    (c2mop:set-funcallable-instance-function fun #'car)
    (lambda-list fun))
=> NIL

#+ecl ; Guard for ECL.
#?(let ((fun (make-instance 'fun)))
    (c2mop:set-funcallable-instance-function fun #'car)
    (lambda-list fun))
:signals error

(requirements-about GLOBAL-SYMBOL-P :doc-type function)

;;;; Description:
; tests arg is global-symbol or not.
#?(global-symbol-p '*package*) => T
#?(let (local)
    (declare (ignore local))
    (global-symbol-p 'local)) => NIL
#?(global-symbol-p 'car) => NIL
#?(global-symbol-p :key) => T

#+syntax
(GLOBAL-SYMBOL-P symbol) ; => result

;;;; Arguments and Values:

; symbol := symbol which is global symbol.

; result := boolean

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about SPECIAL-SYMBOL-P :doc-type function)

;;;; Description:
; Tests arg is special symbol or not.

#+syntax
(SPECIAL-SYMBOL-P symbol) ; => result
#?(special-symbol-p '*package*) => T
#?(special-symbol-p 'car) => NIL
#?(special-symbol-p :keyword) => NIL

;;;; Arguments and Values:

; symbol := symbol, otherwise error.
#?(special-symbol-p "not symbol") :signals error

; result := boolean

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about TYPE-EXPAND :doc-type function)

;;;; Description:
; expand user defined type specifier.
#?(deftype function-name ()
    '(or function symbol))
=> FUNCTION-NAME
#?(type-expand 'function-name) => (OR FUNCTION SYMBOL)
,:test equal

#+syntax
(TYPE-EXPAND type) ; => result

;;;; Arguments and Values:

; type := type-specifier, otherwise return arg itself.
#?(type-expand "not type specifier") => "not type specifier"
,:test string=

; result := 2 values
; 1. type specifier which may expanded.
; 2. boolean which represents type specifier is expanded or not.
#?(type-expand 'function-name)
:values ((OR FUNCTION SYMBOL)
	 T)
#?(type-expand 'symbol)
:values (SYMBOL
	 NIL)

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about TYPE-SPECIFIER-P :doc-type function)

;;;; Description:
; Tests arg is type-specifier or not.
#?(type-specifier-p 'function-name) => T
#?(type-specifier-p :not-type-specifier) => NIL
#?(type-specifier-p '(unsigned-byte 8)) => T
#?(type-specifier-p '(mod 4)) => T
#?(type-specifier-p '(not symbol)) => T
#?(type-specifier-p '(integer 4 9)) => T
#?(type-specifier-p '(or symbol string)) => T
#?(type-specifier-p '(eql 4)) => T
#?(type-specifier-p '(satisfies listp)) => T
#?(type-specifier-p '(and list (not null))) => T

;; Added due to CCL specific issue.
#?(type-specifier-p '(keyword string)) => NIL

;; When CCL is Fixed, code should be modified.
#+ccl
#?(ccl:type-specifier-p '(keyword string)) => KEYWORDP

#+syntax
(TYPE-SPECIFIER-P type) ; => result

;;;; Arguments and Values:

; type := any lisp object.

; result := boolean

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

