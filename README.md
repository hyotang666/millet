# MILLET 0.0.18
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

## Known altenatives.
### function-name
* clod - Autodoc generator. Internals.
* [swank](https://github.com/slime/slime) - Backend server of SLIME. Internals.

### lambda-list
* [trivial-arguments](https://github.com/Shinmera/trivial-arguments) - Retrieve the lambda-list of a functioni. Externals.
* clod - Autodoc generator. Internals.
* [swank](https://github.com/slime/slime) - Backend server of SLIME. Internals.

### type-specifier-p
* [trivial-types](https://github.com/m2ym/trivial-types) - Trivial type defintions. Externals.
* [swank](https://github.com/slime/slime) - Backend server of SLIME. Internals.

### type-expand
* [trivial-types](https://github.com/m2ym/trivial-types) - Trivial type defintions. Externals.
* [introspect-environment](https://github.com/Bike/introspect-environment) - Portable but nonstandard introspection of CL environments. Externals.

### global-symbol-p
* [definitions](https://github.com/Shinmera/definitions) - General definitions reflection library. Internals.
* clod - Autodoc generator. Internals.

## From developer
### Product's goal
?
### License
MIT

### Tested with
* SBCL/2.0.9
* CCL/1.12
* ECL/20.4.24
* CLISP/2.49

### Partially supported.
#### lispworks

- [ ] function-name
- [x] lambda-list
- [ ] global-symbol-p
- [ ] special-symbol-p
- [x] type-expand
- [x] type-specifier-p
- [ ] test

Special thanks to Yehouda about #1!
