(defpackage :millet.spec (:use :cl :jingoh :millet))
(in-package :millet.spec)
(setup :millet)

(requirements-about FUNCTION-NAME)

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
#?(function-name #'(lambda()(print :hoge)))
=> NIL
#?(function-name (flet((test():hoge))#'test)) => TEST
#?(function-name (labels((test():hoge))#'test)) => TEST

; CCL specific issue. (macro) returns (:INTERNAL HOGE MACRO).
#?(defmacro macro()
    (flet((hoge()))
      `',(function-name #'hoge)))
=> MACRO
,:before (fmakunbound 'macro)

#?(macro)
=> HOGE

; When setf function comes, (SETF NAME) is returned.
#?(defclass foo () ((bar :accessor foo-bar)))
:satisfies (lambda($arg)
	     (& (typep $arg 'standard-class)
		(eq 'foo (class-name $arg))))
,:before (mapc #'fmakunbound '(foo-bar (setf foo-bar)))

#?(function-name (fdefinition '(setf foo-bar)))
=> (SETF FOO-BAR)
,:test equal

;;;; Exceptional-Situations:

(requirements-about LAMBDA-LIST)

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

(requirements-about GLOBAL-SYMBOL-P)

;;;; Description:
; tests arg is global-symbol or not.
#?(global-symbol-p '*package*) => T
#?(let(local)
    (declare(ignore local))
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

(requirements-about SPECIAL-SYMBOL-P)

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

(requirements-about TYPE-EXPAND)

;;;; Description:
; expand user defined type specifier.
#?(deftype function-name()
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

(requirements-about TYPE-SPECIFIER-P)

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

