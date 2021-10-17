(in-package :millet)

;;;; Stolen from swank.

(defun function-name (function)
  (cond
    ((eval:interpreted-function-p function)
     (eval:interpreted-function-name function))
    ((pcl::generic-function-p function) (pcl::generic-function-name function))
    ((c::byte-function-or-closure-p function) (c::byte-function-name function))
    (t (kernel:%function-name (kernel:%function-self function)))))

(defun lambda-list (arg)
  (etypecase arg
    ((cons (eql lambda)) (second arg))
    ((cons (eql setf)) (function-arglist (fdefinition arg)))
    (function (function-arglist arg))
    (symbol
     (function-arglist (or (macro-function arg) (symbol-function arg))))))

(defun make-arg-symbol (i) (make-symbol (format nil "~A~D" (string 'arg) i)))

(defmacro with-struct ((conc-name &rest names) obj &body body)
  "Like with-slots but works only for structs."
  (check-type conc-name symbol)
  (flet ((reader (slot)
           (intern
             (concatenate 'string (symbol-name conc-name) (symbol-name slot))
             (symbol-package conc-name))))
    (let ((tmp (gensym "OO-")))
      `(let ((,tmp ,obj))
         (symbol-macrolet ,(loop for name in names
                                 collect (typecase name
                                           (symbol
                                            `(,name (,(reader name) ,tmp)))
                                           (cons
                                            `(,(first name)
                                              (,(reader (second name)) ,tmp)))
                                           (t
                                            (error
                                              "Malformed syntax in WITH-STRUCT: ~A"
                                              name))))
           ,@body)))))

;;; A "hairy" byte-function is one that takes a variable number of
;;; arguments. `hairy-byte-function' is a type from the bytecode
;;; interpreter.
;;;

(defun hairy-byte-function-arglist (fn)
  (let ((counter -1))
    (flet ((next-arg ()
             (make-arg-symbol (incf counter))))
      (with-struct
        (c::hairy-byte-function- min-args max-args rest-arg-p keywords-p
                                 keywords)
        fn
        (let ((arglist 'nil) (optional (- max-args min-args)))
          ;; XXX isn't there a better way to write this?
          ;; (Looks fine to me. -luke)
          (dotimes (i min-args) (push (next-arg) arglist))
          (when (plusp optional)
            (push '&optional arglist)
            (dotimes (i optional) (push (next-arg) arglist)))
          (when rest-arg-p
            (push '&rest arglist)
            (push (next-arg) arglist))
          (when keywords-p
            (push '&key arglist)
            (loop for (key _ __) in keywords
                  do (push key arglist))
            (when (eq keywords-p :allow-others)
              (push '&allow-other-keys arglist)))
          (nreverse arglist))))))

;;; Deriving arglists for byte-compiled functions:
;;;

(defun byte-code-function-arglist (fn)
  ;; There doesn't seem to be much arglist information around for
  ;; byte-code functions.  Use the arg-count and return something like
  ;; (arg0 arg1 ...)
  (etypecase fn
    (c::simple-byte-function
     (loop for i from 0 below (c::simple-byte-function-num-args fn)
           collect (make-arg-symbol i)))
    (c::hairy-byte-function (hairy-byte-function-arglist fn))
    (c::byte-closure
     (byte-code-function-arglist (c::byte-closure-function fn)))))

;;; A simple case: the arglist is available as a string that we can
;;; `read'.

(defun read-arglist (fn)
  "Parse the arglist-string of the function object FN."
  (let ((string (kernel:%function-arglist (kernel:%function-self fn)))
        (package
         (find-package
           (c::compiled-debug-info-package
             (kernel:%code-debug-info (vm::find-code-object fn))))))
    (with-standard-io-syntax
     (let ((*package* (or package *package*)))
       (read-from-string string)))))

;;; A harder case: an approximate arglist is derived from available
;;; debugging information.

(defun debug-variable-symbol-or-deleted (var)
  (etypecase var
    (di:debug-variable (di::debug-variable-symbol var))
    ((member :deleted) '#:deleted)))

(defun debug-function-arglist (debug-function)
  "Derive the argument list of DEBUG-FUNCTION from debug info."
  (let ((args (di::debug-function-lambda-list debug-function))
        (required 'nil)
        (optional 'nil)
        (rest 'nil)
        (key 'nil))
    ;; collect the names of debug-vars
    (dolist (arg args)
      (etypecase arg
        (di::debug-variable (push (di::debug-variable-symbol arg) required))
        ((member :deleted) (push ':deleted required))
        (cons
         (ecase (car arg)
           (:keyword (push (second arg) key))
           (:optional
            (push (debug-variable-symbol-or-deleted (second arg)) optional))
           (:rest
            (push (debug-variable-symbol-or-deleted (second arg)) rest))))))
    ;; intersperse lambda keywords as needed
    (append (nreverse required)
            (if optional
                (cons '&optional (nreverse optional)))
            (if rest
                (cons '&rest (nreverse rest)))
            (if key
                (cons '&key (nreverse key))))))

(defun function-arglist (fun)
  (let ((arglist
         (cond
           ((eval:interpreted-function-p fun)
            (eval:interpreted-function-arglist fun))
           ((pcl::generic-function-p fun)
            (pcl:generic-function-lambda-list fun))
           ((c::byte-function-or-closure-p fun)
            (byte-code-function-arglist fun))
           ((kernel:%function-arglist (kernel:%function-self fun))
            (handler-case (read-arglist fun)
              (error ()
                :not-available)))
           ;; this should work both for compiled-debug-function
           ;; and for interpreted-debug-function
           (t
            (handler-case
                (debug-function-arglist (di::function-debug-function fun))
              (di:unhandled-condition ()
                :not-available))))))
    (check-type arglist (or list (member :not-available)))
    arglist))

(defun global-symbol-p (symbol)
  (or (constantp symbol) (walker:variable-globally-special-p symbol)))

(defun special-symbol-p (symbol) (walker:variable-globally-special-p symbol))

(defun type-expand (type)
  (let ((expand? (kernel:type-expand type)))
    (if (eq expand? type)
        (values type nil)
        (values expand? t))))

(defun type-specifier-p (specifier)
  (handler-case (progn (kernel:specifier-type specifier) t)
    ((or error kernel:parse-unknown-type) ()
      nil)))