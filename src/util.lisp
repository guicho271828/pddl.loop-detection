(in-package :pddl.loop-detection)
(use-syntax :annot)

(defun force (thunk)
  (if (functionp thunk)
      (funcall thunk)
      thunk))

@export
(defmacro forcef (place)
  (multiple-value-bind (vars vals store-vars writer reader)
      (get-setf-expansion place)
    @ignorable reader
    `(let* (,@(iter (for var in vars)
                    (for val in vals)
                    (collect `(,var ,val)))
            (,(car store-vars) (force ,reader)))
       ,writer)))


@export
(defmacro lcons (a b)
  "cons whose cdr is lazy"
  `(cons (lambda () ,a)
         (lambda () ,b)))

@export
(defmacro llist (a &rest args)
  "list using lcons"
  `(lcons ,a ,(when args `(llist ,@args))))

@export
(defmacro ltree (tree)
  (if (consp tree)
      `(llist ,@(mapcar (lambda (e) `(ltree ,e)) tree))
      tree))

@export
(defun fcdr (lcons)
  (forcef (cdr lcons)))

@export
(defun fcar (lcons)
  (forcef (car lcons)))

@export
(defmacro fpop (place)
  (multiple-value-bind (vars vals store-vars writer reader)
      (get-setf-expansion place)
    @ignorable reader writer
    `(let* (,@(iter (for var in vars)
                    (for val in vals)
                    (collect `(,var ,val)))
            (,(car store-vars) (force (cdr ,reader))))
       (prog1
           (fcar ,reader)
         ,writer))))

;; (fpop (aref a i j))
;; (fpop (first (progn (incf i) a)))
