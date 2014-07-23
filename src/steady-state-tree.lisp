(in-package :pddl.loop-detection)
(use-syntax :annot)

;;;; utilities

@export
(defun make-eol (steady-state length)
  "make the end-of-loop state"
  `(,@(cdr steady-state) ,length))

(declaim (ftype (function (list list) boolean) mutices-no-conflict-p))
@export
(defun conflict-p (resources1 resources2)
  (dolist (e1 resources1 nil)
    (dolist (e2 resources2)
      (when (eqstate e1 e2)
        (return-from conflict-p t)))))

;;;; tree version

#|
 (0 (1 2 3 4)
    (2 3))
represents the following steady-states:

 (0)
 (0 1)
 (0 1 2)
 (0 1 3)
 (0 1 4)
 (0 2)
 (0 2 3)

the total consing above is 4+2+3=9 and 17 respectively.
the number of conses is different between each other by an order of magnitude.
for each leaf node, the first structure requires only one cons, while 
the second requires N conses where N is the length of a steady state.
|#


(progn
  (define-local-function %leaf (this now i)
    (if (conflict-p this now) nil i))

  (define-local-function %tree-rec (ms now i)
    ;; now is a list of resources in use in the current branch of the
    ;; tree. For example, if it is in the (0 (1)) position in the example
    ;; above, it is using the locks acquired in both 0th and the 1st
    ;; position. It only adds the elements.
    (match ms
      ((list (movement _ this)) ; only one step remains
       (%leaf this now i))
      ((list* (movement _ this) rest)
       (unless (conflict-p this now)
         (let ((next (append this now)) ; next resources
               (len (length rest)))
           (cons i
                 (iter (for rest2 on rest)
                       (for next-i from (+ 1 i) to (+ 1 i len))
                       (for child = (%tree-rec rest2 next next-i))
                       ;; if conflict has happened, skip it
                       (when child
                         (collecting child)))))))))

  @export @doc "Takes a list of movements.
Returns a cons tree of steady states. Each steady state is
represented by a leaf or a branch of the tree. Each leaf or a branch node
is a mutex position index."
  (defun steady-state-tree (movements)
    (more-labels () (%tree-rec %leaf)
      (match movements
        ((list* (movement _ resources) rest)
         (%tree-rec rest resources 0))))))

;;;; lazy tree version

(progn
  (define-local-function %lazy-rec (ms now i)
    ;; now is a list of resources in use in the current branch of the
    ;; tree. For example, if it is in the (0 (1)) position in the example
    ;; above, it is using the locks acquired in both 0th and the 1st
    ;; position. It only adds the elements.
    (match ms
      ((list (movement _ this)) ; only one step remains
       (%leaf this now i))
      ((list* (movement _ this) rest)
       (unless (conflict-p this now)
         (let ((next (append this now)) ; next resources
               (len (length rest)))
           (lcons i
                  (iter (for rest2 on rest)
                        (for next-i from (+ 1 i) to (+ 1 i len))
                        (for child = (%tree-rec rest2 next next-i))
                        ;; if conflict has happened, skip it
                        (when child
                          (collecting child)))))))))

  @export @doc "Takes a list of movements.
Returns a cons tree of steady states. Each steady state is
represented by a leaf or a branch of the tree. Each leaf or a branch node
is a mutex position index."
  (defun steady-state-lazy (movements)
    (more-labels () (%tree-rec %leaf)
      (match movements
        ((list* (movement _ resources) rest)
         (%tree-rec rest resources 0))))))
