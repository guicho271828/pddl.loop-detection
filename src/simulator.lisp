(in-package :pddl.loop-detection)
(use-syntax :annot)

@export
(defclass state-node (searchable-node)
  ((current-state :initarg :current-state :reader current-state)
   (goal :initarg :goal :accessor goal)
   (movements :initarg :movements :reader movements)
   (complementary-edge-class :initform 'transition)))

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

@export
(defclass transition (searchable-edge)
  ((complementary-node-class :initform 'state-node)))

(defmethod generic-eq ((n1 state-node) (n2 state-node))
  (equalp (current-state n1)
	  (current-state n2)))

(defmethod cost ((tr transition))
  1)
(defmethod heuristic-cost-between ((n1 state-node) (n2 state-node))
  ;; this value is always the same, so it is meaningless.
  ;; (iter (for pos1 in (current-state n1))
  ;; 	(for pos2 in (current-state n2))
  ;; 	(summing (abs (- pos2 pos1))))
  0
  )

(defmethod constraint-ordering-op ((n state-node))
  0) ;; it was found to have no effect if any.

@export
(defun search-loop-path (movements-shrinked steady-state &key (verbose t))
  (handler-return ((path-not-found (lambda (c)
				     @ignore c
				     nil)))
    (let* ((*state-hash* (make-hash-table :test #'equalp))
           (goal (make-instance
                  'state-node
                  :movements movements-shrinked
                  :current-state (make-eol steady-state
                                           (length movements-shrinked))))
           (start (make-instance
                   'state-node
                   :movements movements-shrinked
                   :goal goal
                   :current-state steady-state)))
      (setf (goal goal) goal)
      (let ((last (a*-search start goal :verbose verbose)))
        (iter (for node first last then (parent node))
              (while node)
              (collect (current-state node) at beginning))))))

(defun %make-state-node (movements-shrinked current-state goal n)
  (make-instance
   'state-node
   :goal goal
   :movements movements-shrinked
   :current-state (substitute (1+ n) n current-state)))

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
	   (nth n2 movements)
	   (set-difference used (nth n movements)
			   :test #'eqstate)
	   :test #'eqstate))))))

(defmethod generate-nodes ((state state-node))
  (with-slots (movements current-state) state
    ;; movements: (mutex*)*
    ;; current: number*
    (let* ((goal (goal state))
	   (goal-state (current-state goal))
	   (used (mappend (rcurry #'nth movements) current-state)))
      ;; used: mutices currently in use
      (mapcar
       (curry #'%make-state-node movements current-state goal)
       (iter (for n in current-state)
	     (for goal-n in goal-state)
	     (when (movable n goal-n used movements)
	       (collect n)))))))

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

(defun %report-duplication (ss duplicated)
  @ignore duplicated
  (format t 
          "~w is not searched because it had appeared in the other loop."
          ss))

(defun %report-results (max fdup-count pdup-count total-count)
  (format t
          "~%~1,80,80,'-a~%~{~80@<~40@a | ~10:a~>~%~}~1,80,80,'-a"
          "-"
          (list "All steady-states" max
                "Duplicated loops forward-detected" fdup-count
                "Duplicated loops post-detected" pdup-count
                "Valid loops in total" total-count
                "Valid loops w/o duplicated ones" (- total-count
                                                     fdup-count
                                                     pdup-count))
          "-"))

(defun make-buckets (n)
  (make-array n :initial-element nil))

(defun %forward-duplication-check (ss end bucket)
  (some
   (lambda (loop-path)
     (or (find ss loop-path :test #'equalp)
         (find end loop-path :test #'equalp)))
   bucket))

(defun %post-duplication-check (buckets)
  (let ((count 0))
    (values
     (map 'vector
          (lambda (bucket)
            (multiple-value-bind (result dup-count)
                (%post-duplication-check/bucket bucket)
              (incf count dup-count)
              result))
          buckets)
     count)))

(defun %post-duplication-check/bucket (bucket)
  (let ((shrinked (remove-duplicates bucket :test #'%loop-equal)))
    (values shrinked
            (- (length bucket)
               (length shrinked)))))

(defun %loop-equal (path1 path2)
  (or (find (first-elt path1) path2 :test #'equalp)
      (find (last-elt path1) path2 :test #'equalp)
      (find (first-elt path2) path1 :test #'equalp)
      (find (last-elt path2) path1 :test #'equalp)
      ))

(setf lparallel:*kernel* (make-kernel (get-core-num)))

@export
(defun exploit-loopable-steady-states (movements-shrinked steady-states
                                       &key (verbose t))
  "Returns the list of solution path from start-of-loop to end-of-loop.
start-of-loop is always the same list to the steady-state in the
meaning of EQUALP."
  
  (case verbose
    ((2 t)       (%verbose movements-shrinked steady-states))
    ((1 :modest) (%modest movements-shrinked steady-states))
    (otherwise   (%none movements-shrinked steady-states))))

(macrolet ((%loop-verbosity (before
                             after-success
                             after-failure
                             search-argument
                             if-duplicated)
             `(let ((m-num (length moves))
                    (buckets (make-buckets m-num))
                    (max (length steady-states))
                    (fdup-count 0)
                    (total-count 0))
                (iter 
                  (for ss in steady-states)
                  (for bases = (1- (length ss)))
                  ,before
                  (if-let ((duplicated
                            (%forward-duplication-check
                             ss (make-eol ss m-num) (aref buckets bases))))
                    (progn
                      (incf fdup-count)
                      (incf total-count)
                      ,if-duplicated)
                    (if-let ((result
                              (search-loop-path
                               moves ss :verbose ,search-argument)))
                      (progn
                        ,after-success
                        (incf total-count)
                        (push result (aref buckets bases)))
                      (progn 
                        ,after-failure
                        (collect ss into invalid-loops))))
                  (finally
                   (multiple-value-bind (result pdup-count)
                       (%post-duplication-check buckets)
                     (%report-results max fdup-count pdup-count total-count)
                     (return
                       (values (reduce #'append result) invalid-loops))))))))
  
  (defun %verbose (moves steady-states)
    (let ((i 0))
      (%loop-verbosity
       (progn (format t "~%~a/~a: " i max) (incf i))
       (format t " ...success!")
       (format t " ...failed.")
       t
       (%report-duplication ss duplicated))))

  (defun %modest (moves steady-states)
    (let ((i 0))
      (%loop-verbosity
       (progn (when (zerop (mod i 60)) (terpri)) (incf i))
       (write-char #\.)
       (write-char #\F)
       nil
       (write-char #\D))))

  (defun %none (moves steady-states)
    (%loop-verbosity nil nil nil nil nil)))

