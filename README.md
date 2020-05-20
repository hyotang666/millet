# MILLET 0.0.12
## Current lisp world
Alexandria covers many utilities.
## Issues
Alexandria says never use implementation dependent features.
But some utilities need to depend on it.
## Proposal
Millet covers it.

## Usage

```lisp
(function-name #'car) => CAR
(lambda-list #'car) => (#:ARG0)
(global-symbolp 'pi) => T
```

## From developer
### Product's goal
?
### License
MIT

### Tested with
* SBCL/2.0.2
* CCL/1.12
* ECL/20.4.24
* CLISP/2.49
