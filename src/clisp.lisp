(in-package :millet)

(defun function-name (function)
  (check-type function function)
  (let ((it (sys::function-name function)))
    (etypecase it
      (symbol
       (if (eq :lambda it)
           nil
           (and (symbol-package it) it))) ; when compile time,
                                          ; sys::function-name return
                                          ; uninterned symbol which bound by
                                          ; lambda function.
      ((cons (eql setf) t) it)))) ; for (setf foo)

(defun global-symbol-p (symbol)
  (check-type symbol symbol)
  (ext:special-variable-p symbol))

(defun special-symbol-p (symbol)
  (and (global-symbol-p symbol) (not (constantp symbol))))

(defun lambda-list (arg) (ext:arglist arg))

(defun type-expand (type)
  (handler-case (ext:type-expand type)
    (error ()
      (values type nil))))