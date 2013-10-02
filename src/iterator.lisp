(in-package :pddl.loop-detection)
(use-syntax :annot)

#|

 (0 (1 2 3))

iterator yields:

 (0)
 (0 1)
 (0 1 2)
 (0 1 3)

|#

@export
(define-condition tree-exhausted (condition)
  ())

@export
@doc "depth first tree iterator"
(defun tree-iterator (tree &key lazy)
  (if lazy
      (lazy-tree-iterator tree)
      (regular-tree-iterator tree)))

(defun regular-tree-iterator (tree)
  (let ((first t)
        env
        stack
        (lock (make-lock "Tree lock")))
    (lambda (&key wind-stack get-current-stack)
      (with-lock-held (lock)
        (cond
          (get-current-stack
           stack)
          (wind-stack
           ;; (format t "~&winding stack until ~w" wind-stack)
           (if (find wind-stack stack)
               (iter (until (eql wind-stack (car stack)))
                     ;; (format t "~&~4twinding ~w" (car stack))
                     (pop stack)
                     (pop env)
                     (finally
                      ;; (format t "~&~4twinding ~w" (car stack))
                      (pop env)
                      (pop stack)
                      (return (car stack))))
               ;; (format t "~& stack ~w is not found!" wind-stack)
               ))
          
          ((and (null env) (null stack) first)
           ;; first time
           (push tree stack)
           (push (cdr tree) env)
           (setf first nil)
           (values (list (first tree))
                   (car stack)))
          ((and (null env) (null stack))
           (signal 'tree-exhausted))
          
          ((and env (first env) (consp (first env)) stack)
           (let ((opened (pop (first env))))
             (multiple-value-prog1
                 (cond
                   ((atom opened)
                    (values 
                     (nreverse (cons opened (mapcar #'first stack)))
                     (car stack)))
                   ((consp opened)
                    (push opened stack)
                    (push (cdr opened) env)
                    (values 
                     (nreverse (mapcar #'first stack))
                     (car stack))))
               (iter (while (and env (null (first env))))
                     ;; (sleep 0.2)
                     ;; (format t "~& ~w ~w" env stack)
                     (pop env)
                     (pop stack)))))
          
          ((and env (first env) (atom (first env)) stack)
           ;; backtrack
           (multiple-value-prog1
               (values 
                (nreverse (cons (first env) (mapcar #'first stack)))
                (car stack))
             (pop env)))
          (t (error "no match ~w ~w" env stack)))))))








(defun lazy-tree-iterator (tree)
  "An iterator which assumes every cons of TREE are LAMBDAs.
the child nodes can be obtained by funcalling each LAMBDA."
  (let ((first t)
        env
        stack
        (lock (make-lock "Tree lock")))
    (lambda (&key wind-stack get-current-stack)
      (with-lock-held (lock)
        (cond
          (get-current-stack
           stack)
          (wind-stack
           ;; (format t "~&winding stack until ~w" wind-stack)
           (if (find wind-stack stack)
               (iter (until (eql wind-stack (car stack)))
                     ;; (format t "~&~4twinding ~w" (car stack))
                     (pop stack)
                     (pop env)
                     (finally
                      ;; (format t "~&~4twinding ~w" (car stack))
                      (pop env)
                      (pop stack)
                      (return (car stack))))
               ;; (format t "~& stack ~w is not found!" wind-stack)
               ))
          
          ;; first time
          ((and (null env) (null stack) first)
           (push tree stack)
           (push (fcdr tree) env)
           (setf first nil)
           (values (list (first tree))
                   (car stack)))

          ;; tree exhausted
          ((and (null env) (null stack))
           (signal 'tree-exhausted))
          
          ;; normal propagation
          ((and env (first env) (consp (first env)) stack)
           (let ((opened (fpop (first env))))
             (multiple-value-prog1
                 (cond
                   ((atom opened)
                    (values 
                     (nreverse (cons opened (mapcar #'first stack)))
                     (car stack)))
                   ((consp opened)
                    (push opened stack)
                    (push (fcdr opened) env)
                    (values 
                     (nreverse (mapcar #'first stack))
                     (car stack))))
               (iter (while (and env (null (first env))))
                     ;; (sleep 0.2)
                     ;; (format t "~& ~w ~w" env stack)
                     (pop env)
                     (pop stack)))))
          
          ((and env (first env) (atom (first env)) stack)
           ;; backtrack
           (multiple-value-prog1
               (values 
                (nreverse (cons (first env) (mapcar #'first stack)))
                (car stack))
             (pop env)))
          (t (error "no match ~w ~w" env stack)))))))