(in-package :cl-user)
(defpackage :millet.design(:use :cl :millet :jingoh))
(in-package :millet.design)

(setup :millet)

(requirements-about function-name)

#|
accept function, return its name.
|#
#?(function-name #'car) => car

#|
when lambda function, return NIL.
|#
#?(function-name #'(lambda(arg)(print arg))) => NIL

#|
when setf-expander, return (SETF name).
|#
(defstruct foo bar)
(let((it(ignore-errors(fdefinition'(setf foo-bar)))))
  (when it
    (?(function-name it) => (setf foo-bar):test #'equal)))

#|
when argument is not function, signals an error.
|#
#?(function-name 'car) :signals type-error

(requirements-about lambda-list)

#|
accept function, return its lambda-list.
|#
#?(lambda-list #'(lambda(foo &rest bar)(declare(ignore foo bar))))
=> #.implementation-dependent ; especially lambda form is compiled.

#|
when argument is not function designator, signals an error.
|#
#?(lambda-list 0) :signals type-error

(requirements-about global-symbol-p)

#|
accept symbol, when its defined with defvar defparameter defconstant keyword, returns T. Other symbols are NIL.
|#
#?(global-symbol-p '*package*) => T
#?(global-symbol-p 'pi) => T
#?(global-symbol-p :foo) => T
#?(global-symbol-p 'foo) => NIL
#?(global-symbol-p 0) :signals type-error
