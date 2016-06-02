# [Function] GLOBAL-SYMBOL-P

## Syntax:

(GLOBAL-SYMBOL-P SYMBOL) => result

## Arguments and Values:

result := BOOLEAN

## Description:
When SYMBOL is defined with DEFVAR, DEFPARAMETER, or DEFCONSTANT, evaluated T, otherwise NIL.

## Example:

## Affected-By:

## Side-Effects:

## Notes:

## See-Also:

## Exceptional-Situations:
When argument is not type-of SYMBOL, an error may be signaled (implementation dependent).
