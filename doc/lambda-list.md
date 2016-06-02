# [Function] LAMBDA-LIST

## Syntax:

(LAMBDA-LIST arg) => lambda-list

## Arguments and Values:

arg := function designator

lambda-list := Ordinary lambda list

## Description:

Get lambda list from function designator ARG.
Can handle function, generic-function, lambda-function, macro, but special-operator.

## Example:

## Affected-By:

## Side-Effects:

## Notes:

### Implementation dependent:

* ECL need ARG is not compiled.
Otherwise NIL is returned.

## See-Also:

## Exceptional-Situations:

