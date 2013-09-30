(in-package :pddl.loop-detection)
(use-syntax :annot)

@export
(defun force (thunk)
  (if (functionp thunk)
      (funcall thunk)
      thunk))

@export
(defmacro lcons (a b)
  "cons whose cdr is lazy"
  `(cons ,a (lambda () ,b)))

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
  (let ((val (force (cdr lcons))))
    (setf (cdr lcons) val)
    val))

@export
(defmacro fpop (place)
  (let ((value place))
    (once-only (value)
      `(prog1 (car ,value) (setf ,place (fcdr ,value))))))

;; (pop (first a))