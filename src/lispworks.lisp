(in-package :millet)

(defun lambda-list (arg) (lw:function-lambda-list arg))

(defun type-expand (type) (type:expand-user-type type))

(defun type-specifier-p (type) (type:valid-type-specifier type))