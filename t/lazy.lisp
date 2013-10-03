
(in-package :pddl.loop-detection-test)

(def-suite :pddl.loop-detection :in :pddl)
(in-suite :pddl.loop-detection)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; test tree

(test tree-iterator
  (let ((it (tree-iterator '(0 1 2))))
    (is (equal '(0) (funcall it)))
    (is (equal '(0 1) (funcall it)))
    (is (equal '(0 2) (funcall it))))

  (let ((it (tree-iterator '(0
                             (1 2 3)
                             4
                             (2
                              (3 4 6)
                              (7 8 9))
                             5))))
    (is (equal '(0) (funcall it)))
    (is (equal '(0 1) (funcall it)))
    (is (equal '(0 1 2) (funcall it)))
    (is (equal '(0 1 3) (funcall it)))
    (is (equal '(0 4) (funcall it)))
    (is (equal '(0 2) (funcall it)))
    (is (equal '(0 2 3) (funcall it)))
    (is (equal '(0 2 3 4) (funcall it)))
    (is (equal '(0 2 3 6) (funcall it)))
    (is (equal '(0 2 7) (funcall it)))
    (is (equal '(0 2 7 8) (funcall it)))
    (is (equal '(0 2 7 9) (funcall it)))
    (is (equal '(0 5) (funcall it))))

  (let ((it (tree-iterator '(0
                             (1 2 3)
                             4
                             (2
                              (3 4 6 (7 -1 9) (10 11) 12 13)
                              (7 8 9))
                             5))))
    (is (equal '(0) (funcall it)))
    (is (equal '(0 1) (funcall it)))
    (is (equal '(0 1 2) (funcall it)))
    (is (equal '(0 1 3) (funcall it)))
    (is (equal '(0 4) (funcall it)))
    (is (equal '(0 2) (funcall it)))
    (is (equal '(0 2 3) (funcall it)))
    (multiple-value-bind (value stack) (funcall it)
      (is (equal '(0 2 3 4) value))
      (is (equalp '(3 4 6 (7 -1 9) (10 11) 12 13) stack))
      (is (equalp '(2
                    (3 4 6 (7 -1 9) (10 11) 12 13)
                    (7 8 9))
                  (funcall it :wind-stack stack))))

    (multiple-value-bind (value stack) (funcall it)
      (is (equal '(0 2 7) value))
      (is (equalp '(7 8 9) stack)))
      
    (is (equal '(0 2 7 8) (funcall it)))
    (is (equal '(0 2 7 9) (funcall it)))
    (is (equal '(0 5) (funcall it)))
    (signals tree-exhausted (funcall it))))

(test (lazy-tree-iterator :depends-on tree-iterator)
  (let ((it (tree-iterator (llist 0 1 2) :lazy t)))
    (is (equal '(0) (funcall it)))
    (is (equal '(0 1) (funcall it)))
    (is (equal '(0 2) (funcall it)))
    (signals tree-exhausted (funcall it)))

  (let ((it (tree-iterator (ltree (0
                                   (1 2 3)
                                   4
                                   (2
                                    (3 4 6)
                                    (7 8 9))
                                   5))
                           :lazy t)))
    (is (equal '(0) (funcall it)))
    (is (equal '(0 1) (funcall it)))
    (is (equal '(0 1 2) (funcall it)))
    (is (equal '(0 1 3) (funcall it)))
    (is (equal '(0 4) (funcall it)))
    (is (equal '(0 2) (funcall it)))
    (is (equal '(0 2 3) (funcall it)))
    (is (equal '(0 2 3 4) (funcall it)))
    (is (equal '(0 2 3 6) (funcall it)))
    (is (equal '(0 2 7) (funcall it)))
    (is (equal '(0 2 7 8) (funcall it)))
    (is (equal '(0 2 7 9) (funcall it)))
    (is (equal '(0 5) (funcall it)))
    (signals tree-exhausted (funcall it)))

  (let ((it (tree-iterator (ltree (0
                                   (1 2 3)
                                   4
                                   (2
                                    (3 4 6 (7 -1 9) (10 11) 12 13)
                                    (7 8 9))
                                   5))
                           :lazy t)))
    (is (equal '(0) (funcall it)))
    (is (equal '(0 1) (funcall it)))
    (is (equal '(0 1 2) (funcall it)))
    (is (equal '(0 1 3) (funcall it)))
    (is (equal '(0 4) (funcall it)))
    (is (equal '(0 2) (funcall it)))
    (is (equal '(0 2 3) (funcall it)))
    (multiple-value-bind (value stack) (funcall it)
      (is (equal '(0 2 3 4) value))
      (is-true (ematch stack
                 ((list* 3 4 _) t)))
      (is-true (ematch (funcall it :wind-stack stack)
                 ((list* 2 (list* 3 4 _) _) t))))

    (multiple-value-bind (value stack) (funcall it)
      (is (equal '(0 2 7) value))
      (is-true (ematch stack
                 ((cons 7 _) t))))
      
    (is (equal '(0 2 7 8) (funcall it)))
    (is (equal '(0 2 7 9) (funcall it)))
    (is (equal '(0 5) (funcall it)))
    (signals tree-exhausted (funcall it))))

(defvar steady-state-lazy)
(test (steady-state-lazy :depends-on extract-movements)
  (finishes
    (setf steady-state-lazy
	  (exploit-steady-state-lazy movements-shrinked))))

(defvar loopable-steady-state-lazy)
(test (loopable-steady-state-lazy :depends-on loopable-steady-states)
  (finishes
    (setf loopable-steady-state-lazy
          (exploit-loopable-steady-state-lazy
           movements-shrinked steady-state-lazy :verbose :modest))))

(test integrated-lazy
  (terpri)
  (finishes
    (exploit-loop-problems-lazy cell-assembly-model2b-2-7 'b-0)))