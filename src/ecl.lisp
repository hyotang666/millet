(in-package :millet)

(defun function-name (function)
  (if (eq (type-of function) 'standard-generic-function)
      (clos:generic-function-name function)
      (ext:compiled-function-name function)))

(defun global-symbol-p (symbol)
  (check-type symbol symbol)
  (or (constantp symbol) (walker:variable-special-p symbol nil)))

(defun lambda-list (arg)
  (ext:function-lambda-list
    (typecase arg
      ((cons (eql lambda) *) (coerce arg 'function))
      ((cons (eql setf) *) (fdefinition arg))
      (t arg))))

(defun type-expand (type)
  ;; canonicalize
  ;; [clhs says](http://clhs.lisp.se/Body/04_bc.htm)
  ;; > If a list has one or more unspecified items at the end, those items can be dropped.
  ;; > If dropping all occurrences of * results in a singleton list,
  ;; > then the parentheses can be dropped as well (the list can be replaced by the symbol in its car).
  ;; > For example, (vector double-float *) can be abbreviated to (vector double-float),
  ;; > and (vector * *) can be abbreviated to (vector) and then to vector.
  (when (and (consp type) (null (cdr type)))
    (setq type (car type)))
  (let ((expander
         (si:get-sysprop
           (if (atom type)
               type
               (car type))
           'si::deftype-definition)))
    (if (null expander)
        ;; TYPE is not DEFTYPEed.
        (values type nil)
        (let ((fun-name (function-name expander)))
          (if fun-name
              (if (not
                    (member fun-name '(ext:constantly-t ext:constantly-nil)))
                  ;; ECL/21.2.1 never has named function for DEFTYPE expander.
                  (error "TYPE-EXPAND internal logical error, NIY for ~S ~S"
                         fun-name type)
                  ;; CONSTANTLY is used only deftype lambda-list is null.
                  (if (atom type)
                      ;; No argments. Ok.
                      (values (funcall expander type) t)
                      ;; Some arguments. Not ok.
                      (values type nil)))
              ;; KLUDGE: CONSTANTLY return compiled-closure,
              ;; not compiled-function nor bytecompiled-function.
              (if (search "compiled-closure" (prin1-to-string expander)
                          :start2 2) ; to skip "#<"
                  ;; Expander seems to be made by CONSTANTLY.
                  ;; Deftype lambda-list must be null.
                  (if (atom type)
                      ;; No arguments. Ok.
                      (values (funcall expander type) t)
                      ;; Some arguments, not ok.
                      (values type nil))
                  (handler-case
                      (funcall expander
                               (if (atom type)
                                   nil
                                   (cdr type)))
                    (error ()
                      (values type nil))
                    (:no-error (expanded)
                      (values expanded t)))))))))

(eval-when (:load-toplevel) (incomplete 'type-expand))

(eval-when (:load-toplevel) (incomplete 'type-specifier-p))