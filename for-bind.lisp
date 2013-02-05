
(in-package :cl-user)

(defpackage :smithzv.destructuring-bind-iterate
  (:use :cl :iterate)
  (:shadow #:for)
  (:export 
   #:dfor))

(in-package :smithzv.destructuring-bind-iterate)

(defun argument-keyword-p (sym)
  (member sym '(&rest &optional &aux &key &body &allow-other-keys)))

(defvar *bindings* nil)

(defun record-binding (var)
  (let ((g (gensym)))
    (push (list var g) *bindings*)
    g))

(defun replace-sym-names (lambda-list &optional mode)
  "Scans a destructuring lambda list and extracts any variables that should be
set in a destructuring-bind.  This returns a list of three elements.  The first
is a new lambda list that has the locations that contain binding variable
replaced with gensyms.  The second and third are a list of the symbol names and
the corresponding gensym names, respectively.

This works at arbitrary nesting and with dotted lists, just like destructuring
bind."
  (cond
    ((null lambda-list)
     nil)
    ((atom lambda-list)
     (record-binding lambda-list))
    ((let ((first (first lambda-list)))
       (case first
         ((&optional &aux &key)
          (cons first (replace-sym-names (rest lambda-list) first)))
         ((&body &rest &allow-other-keys)
          (cons first (replace-sym-names (rest lambda-list) mode)))
         (otherwise
          (cons
           (case mode
             ((&optional &aux)
              (cond ((and (consp first)
                          (consp (first first)))
                     ;; Handle bindings that look like: (x (y z))
                     (list (replace-sym-names (first first) nil)
                           (second first)))
                    ((consp first)
                     ;; Handle bindings that look like: (x [default])
                     (list (record-binding (first first)) (second first)))
                    ;; Handle bindings that look like: x
                    (t (record-binding first))))
             ((&key)
              (cond ((and (consp first)
                          (consp (first first))
                          (consp (second (first first))))
                     ;; Handle bindings that look like:
                     ;;           ((:x (y z)) [default] [supplied-p])
                     (list* (list (first (first first))
                                  (replace-sym-names (second (first first)) nil))
                            (second first)
                            (if (third first)
                                (list (record-binding (third first)))
                                nil)))
                    ((and (consp first)
                          (consp (first first)))
                     ;; Handle bindings that look like: ((:x y) [default] [supplied-p])
                     (list* (list (first (first first))
                                  (record-binding (second (first first))))
                            (second first)
                            (if (third first)
                                (list (record-binding (third first)))
                                nil)))
                    ((consp first)
                     ;; Handle bindings that look like: (x [default] [supplied-p])
                     (list* (list (intern (symbol-name (first first)) :keyword)
                                  (record-binding (first first)))
                            (second first)
                            (if (third first)
                                (list (record-binding (third first)))
                                nil)))
                    ;; Handle bindings that look like: x
                    (t (list (list (intern (symbol-name first) :keyword)
                                   (record-binding first))))))
             (otherwise
              (cond ((consp first)
                     ;; Handle bindings that look like: (x y z)
                     (replace-sym-names first nil))
                     ;; Handle bindings that look like: x
                    (t (record-binding first)))))
           (replace-sym-names (rest lambda-list) mode))))))))

(defmacro for (&rest args)
  (destructuring-bind (binding in &rest rest) args
    (if (or (atom binding)
            (not (member (symbol-name in)
                         (list (symbol-name :in)
                               (symbol-name :in-sequence)
                               (symbol-name :in-vector))
                         :test #'equal)))
        ;; Pass-through to normal behavior
        `(iter:for ,@args)
        (let* ((*bindings* nil)
               (binding-var (gensym))
               (d-list (replace-sym-names binding))
               (bindings (reverse *bindings*)))
          `(progn
             ,@(mapcar (lambda (x) `(with ,(first x))) bindings)
             (iter:for ,binding-var ,in ,@rest)
             (destructuring-bind ,d-list ,binding-var
               ,@(mapcar (lambda (binding) (cons 'setf binding)) bindings)))))))

(defmacro dfor (&rest args)
  `(for ,@args))
