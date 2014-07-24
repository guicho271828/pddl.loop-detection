(in-package :pddl.loop-detection)
(use-syntax :annot)

;;;; search node definitions

@export
(defclass state-node (unit-cost-node)
  ((current-state :initarg :current-state :reader current-state)
   (goal :initarg :goal :accessor goal)
   (movements :initarg :movements :reader movements)))

(defvar *state-hash*)
(defmethod allocate-instance :around
    ((class (eql (find-class 'state-node)))
     &key current-state goal)
  (or (gethash current-state *state-hash*)
      (let ((result (call-next-method)))
        (setf (gethash current-state *state-hash*) result)
        result)))

(defmethod print-object ((n state-node) s)
  (print-unreadable-object (n s :type t)
    (with-slots (current-state) n
      (format s "~a :cost ~a" current-state (cost n)))))

(defmethod generic-eq ((n1 state-node) (n2 state-node))
  (equal (current-state n1)
         (current-state n2)))

(defmethod heuristic-cost-between ((n1 state-node) (n2 state-node))
  ;; +this value is always the same, so it is meaningless.+
  ;; FALSE the above description is false.
  ;; actually, for the plan that allowed a base to move in an non-optimal manner
  ;; the truth is that THE BASE SHOULD GO BACKWARD after that wrong move.
  ;; 
  ;; So, the heuristic function is still possible.

  ;; the heuristics below can be considered as an h+ heuristics.
  ;; (declare (optimize (speed 3) (debug 0) (safety 0)))
  ;; (let ((sum 0))
  ;;   (declare (type fixnum sum))
  ;;   (loop
  ;;      for pos1 fixnum in (current-state n1)
  ;;      for pos2 fixnum in (current-state n2)
  ;;      do (setf sum (the fixnum
  ;;                     (+ sum (the fixnum
  ;;                              (abs (the fixnum
  ;;                                     (- pos2 pos1))))))))
  ;;   sum)
  0
  )

(defmethod constraint-ordering-op ((n state-node))
  0) ;; it was found to have no effect if any.

;;;; wrapper function

(defun get-loop-from-last (last)
  (iter (for node first last then (parent node))
        (while node)
        (collect (current-state node) at beginning)))

@export
(defun mutex-focused-planning (movements steady-state &key (verbose t))
  "returns two values: list of paths found, and the cost
 (which is always the same as long as the path exist)"
  (let* ((*state-hash* (make-hash-table :test #'equal))
         (goal (make-instance
                'state-node
                :movements movements
                :current-state (make-eol steady-state
                                         (length movements))))
         (start (make-instance
                 'state-node
                 :movements movements
                 :goal goal
                 :current-state steady-state)))
    (setf (goal goal) goal)
    (let (solutions (cost MOST-POSITIVE-FIXNUM))
      (handler-bind
          ((path-not-found
            (lambda (c)
              (declare (ignorable c))
              (when verbose
                (format t "~&Completely searched the state space!~%Paths found:~%~w"
                        solutions))))
           (solution-found
            (lambda (c)
              (let* ((last (solution c))
                     (path (get-loop-from-last last)))
                (when verbose
                  (format
                   t "~%Cost : ~w~%Solution : ~w"
                   (cost last) path))
                (unless solutions
                  (setf cost (cost last)))
                (push path solutions)
                (continue)))))
        (a*-search-clos start goal :verbose verbose))
      (values solutions cost))))



(declaim (ftype (function (fixnum fixnum list list) boolean) movable)) 
(defun movable (n goal-n used movements)
  (let ((n2 (1+ n))
        (carry-out-index (length movements)))
    (and ; include n if
     (not (= n goal-n)) ; the goal is not achieved yet
     (< n carry-out-index) ; n is not already the carry-out.
     (if (= carry-out-index n2) ; check the resource conflict.
         t          ; if n+1 is the carry-out, it doesn't consume resources.
         (null      ; else, the resource constraint should hold
          (intersection
           (movement-resources (nth n2 movements))
           (set-difference used (movement-resources (nth n movements))
                           :test #'eqstate)
           :test #'eqstate))))))

(defmethod generate-nodes ((state state-node))
  (with-slots (movements current-state) state
    ;; movements: (mutex*)*
    ;; current: number*
    (let* ((goal (goal state))
           (goal-state (current-state goal))
           (used (iter (for index in current-state)
                       (appending
                        (movement-resources
                         (nth index movements))))))
      ;; used: the sum of resources currently in use
      (iter (for n in current-state)
            (for goal-n in goal-state)
            (when (movable n goal-n used movements)
              (collecting
               (%make-state-node movements current-state goal n)))))))

(defun %make-state-node (movements current-state goal n)
  (make-instance
   'state-node
   :goal goal
   :movements movements
   :current-state (substitute (the fixnum (1+ n)) n current-state)))

#|

steady-states, or ss: list of positions of bases like

 (0 3 5 16 19) ; the index of mutex aquired by base 0, 1, 2, 3, 4 

loop-path: list of steady-states, like

 ((0 3 5 16 19)
  (0 3 5 17 19)
  (0 3 6 17 19) ...)

bucket: list of loop-pathes. In each loop-path,
  all steady-states have the same number of bases.

  (((0 3 5 16 19) ; loop path 1
    (0 3 5 17 19) ; loop path 1
    (0 3 6 17 19) ...)  ; loop path 1
   ((0 4 5 16 19) ; loop path 2
    (0 4 5 17 19) ; loop path 2
    (0 4 6 17 19) ...)  ; loop path 2
     ... )

buckets: array of buckets. Each index corresponds to
the number of bases.

|#

(defun %report-results (max fdup-count pdup-count true-count)
  (format t
          "~%~1,80,80,'-a~%~{~80@<~40@a | ~10:a~>~%~}~1,80,80,'-a"
          "-"
          (list "All steady-states" max
                "Duplicated loops forward-detected" fdup-count
                "Duplicated loops post-detected" pdup-count
                "Valid loops in total" (+ true-count
                                          fdup-count
                                          pdup-count)
                "Valid loops w/o duplicated ones" true-count)
          "-"))

(defun density-char (n)
  (case n
    (0 #\Space)
    (1 #\.)
    (2 #\,)
    (3 #\:)
    (4 #\;)
    (5 #\i)
    (6 #\j)
    (7 #\!)
    (8 #\|)
    (9 #\l)
    (10 #\I)
    (11 #\k)
    (12 #\K)
    (13 #\M)
    (14 #\W)
    (15 #\#)
    (16 #\0)
    (17 #\@)
    (t  #\■)))

