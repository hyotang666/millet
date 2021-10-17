# MILLET 0.0.18
Wrapper of implementation dependent tiny utilities.

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
* ECL/20.4.24 ; Failed.
* CLISP/2.49
* Allegro/10.1
* CLASP/2021-05-26 ; Failed.
* CMUCL/21D
* ABCL/1.8.0 ; Failed.

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

#### ABCL

- [x] function-name
- [x] lambda-list
- [x] global-symbol-p
- [x] special-symbol-p
- [x] type-expand
- [ ] type-specifier-p

### Known issues.
#### FUNCTION-NAME
* Local function name is not supported due to ECL and CMUCL specific issue.
#### LAMBDA-LIST
* Common Lisp macro is not supported due to CLISP and CLASP specific issue.
* Local function is not supported due to ECL specific issue.
* The function which is returned from `CL:COMPLEMENT` is not supported due to SBCL and ECL specific issue.
* The function which is returned from `CL:CONSTANTLY` is not supported due to CCL and ECL and CMUCL specific issue.
* Funcallable standard class is not supported due to CCL and ECL and CLASP specific issue.
#### TYPE-SPECIFIER-P
* ECL has [issue](https://gitlab.com/embeddable-common-lisp/ecl/-/issues/570)
* CLASP has same issue.
