# [Function] FUNCTION-NAME

## Syntax:

(FUNCTION-NAME function) => result

## Arguments and Values:

function := function

result := (or null function-designator)

## Description:
Accept function , return its name.

If function is lambda-function, NIL is returned.

If function is setf-expander, list `(setf name)` is returned.

Can handle macro-function, generic-function, setf-expander too.

## Example:

## Affected-By:

## Side-Effects:

## Notes:

## See-Also:

## Exceptional-Situations:
When function is not function object, an error of type type-error is signaled.
