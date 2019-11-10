# MILLET 0.0.9
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
* SBCL/1.5.8
* CCL/1.11.5
* ECL/16.1.3
* CLISP/2.49
