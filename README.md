# MILLET - Tiny utilities which abandoned by alexandria.

* Current lisp world
Alexandria covers many utilities.

* Issues
Alexandria says never use implementation dependent features.
But some utilities need to depends on it.

* Propose
Millet covers it.

## Usage
```lisp
(function #'car) => CAR
(lambda-list #'car) => (#:ARG0)
(global-symbolp 'pi) => T
```

## From developer

* Product's goal - ?
* License - public domain
* Developped with - CLISP
* Tested with - SBCL CCL ECL

