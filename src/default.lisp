(in-package :millet)

(eval-when (:compile-toplevel :load-toplevel)
  ;; In order to distinguish the default functions and implementation-specific functions,
  ;; we set symbol-plist as the mark of default functions.
  ;; Otherwise, when millet is recompiled,
  ;; fasl will lose default functions.
  (defun mark-as-default (symbol)
    "Set the mark of the millet default function to symbol-plist."
    (setf (get symbol 'millet-default) t)
    symbol)
  (defun defaultp (symbol)
    "Is the default function?"
    (get symbol 'millet-default))
  (defun already-defined-p (name)
    (let ((symbol (find-symbol (string name) :millet)))
      (unless symbol
        (error "Missing ~S." symbol))
      (and (fboundp symbol) (not (defaultp symbol))))))

#.(if (already-defined-p :function-name)
      (values)
      `(defun ,(mark-as-default 'function-name) (function)
         (check-type function function)
         (nth-value 2 (function-lambda-expression function))))

#.(if (already-defined-p :global-symbol-p)
      (values)
      `(defun ,(mark-as-default 'global-symbol-p) (symbol)
         (declare (ignore symbol))
         (not-support 'global-symbol-p)))

#.(if (already-defined-p :special-symbol-p)
      (values)
      `(defun ,(mark-as-default 'special-symbol-p) (symbol)
         (and (global-symbol-p symbol) (not (constantp symbol)))))

#.(if (already-defined-p :lambda-list)
      (values)
      `(defun ,(mark-as-default 'lambda-list) (arg)
         (declare (ignore arg))
         (not-support 'lambda-list)))

#.(if (already-defined-p :type-expand)
      (values)
      `(defun ,(mark-as-default 'type-expand) (type)
         (declare (ignore type))
         (not-support 'type-expand)))

#.(if (already-defined-p :type-specifier-p)
      (values)
      `(defun ,(mark-as-default 'type-specifier-p) (type)
         (labels ((rec (specifier)
                    (cond
                      ((symbolp specifier)
                       (and (not (keywordp specifier))
                            (values (ignore-errors
                                     (progn (typep '#:dummy specifier) t)))))
                      ((typep specifier '(cons (eql not)))
                       (and (null (cddr specifier))
                            (type-specifier-p (expand (cadr specifier)))))
                      ((typep specifier '(cons (member and or)))
                       (loop :for spec :in (cdr specifier)
                             :always (rec (expand spec))))
                      ((typep specifier 'standard-class))
                      ((consp specifier)
                       (typecase specifier
                         ((cons
                            (member array base-string bit-vector complex cons
                                    double-float float integer long-float mod
                                    rational real short-float signed-byte
                                    simple-array simple-base-string
                                    simple-bit-vector simple-string
                                    simple-vector single-float string
                                    unsigned-byte vector))
                          (values (ignore-errors
                                   (progn (typep '#:dummy specifier) t))))
                         ((cons (eql satisfies) (cons symbol null))
                          (and (fboundp (cadr specifier)) t))
                         ((cons (eql eql) (cons t null)) t)
                         ((cons (eql member)) t)))
                      (t nil)))
                  (expand (type)
                    (multiple-value-bind (specifier expanded?)
                        (type-expand type)
                      (if expanded?
                          (expand specifier)
                          specifier))))
           (rec (expand type)))))