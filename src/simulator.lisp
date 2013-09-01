(in-package :pddl.loop-detection)
(use-syntax :annot)

@export
(defclass state-node (searchable-node)
  ((current-state :initarg :current-state :reader current-state)
   (goal :initarg :goal :accessor goal)
   (movements :initarg :movements :reader movements)
   (complementary-edge-class :initform 'transition)))

(defvar *movements-hash*
  (make-hash-table :test #'equalp))
(defmethod allocate-instance :around
    ((class (eql (find-class 'state-node)))
     &key movements current-state goal)
  (let ((state-hash
	 (or (gethash movements *movements-hash*)
	     (setf (gethash movements *movements-hash*)
		   (make-hash-table :test #'equalp)))))
    ;; (when (gethash current-state state-hash)
    ;;   (format t "allocation stopped!"))
    (if-let ((found (gethash current-state state-hash)))
      (progn (setf (goal found) goal)
	     found)
      (setf (gethash current-state state-hash)
	    (call-next-method)))))

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
  (remhash movements-shrinked *movements-hash*)
  (handler-return ((path-not-found (lambda (c)
				     @ignore c
				     nil)))
    (let* ((goal (make-instance
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

(defmethod generate-nodes ((state state-node))
  (with-slots (movements current-state) state
    ;; movements: (mutex*)*
    ;; current: number*
    (let* ((max (length movements))
	   (goal (goal state))
	   (goal-state (current-state goal))
	   (used (mappend (rcurry #'nth movements) current-state)))
      ;; used: mutices currently in use
      (mapcar
       (curry #'%make-state-node movements current-state goal)
       (iter (for n in current-state)
	     (for goal-n in goal-state)
	     (for n2 = (1+ n))
	     (when (and ; include n if
		    (not (= n goal-n)) ; the goal is not achieved yet
		    (< n max) ; n is not already the carry-out.
		    (if (= max n2) ; check the resource conflict.
			t          ; if n+1 is the carry-out, it doesn't consume resources.
			(null      ; else, the resource constraint should hold
			 (intersection
			  (nth n2 movements)
			  (set-difference used (nth n movements)
					  :test #'eqstate)
			  :test #'eqstate))))
	       (collect n)))))))

(defun %report-duplication (ss duplicated)
  @ignore duplicated
  (format t "~w is not searched because it had appeared in the other loop." ss))

(defun %check-duplicate (ss loops)
  (some
   (lambda (path)
     (member ss path :test #'equalp))
   (aref loops (1- (length ss)))))

@export
(defun loopable-steady-states (movements-shrinked steady-states &key (verbose t))
  "Returns the list of solution path from start-of-loop to end-of-loop.
start-of-loop is always the same list to the steady-state in the
meaning of EQUALP."
  (iter (with loops = (make-array (length movements-shrinked) :initial-element nil))
	(with max = (length steady-states))
	(with duplicated-count = 0)
	(with total-count = 0)
	(for i from 0)
	(for ss in steady-states)
	(when verbose
	  (format t "~%~a/~a: " i max))
	(if-let ((duplicated (%check-duplicate ss loops)))
	  (progn
	    (incf duplicated-count)
	    (incf total-count)
	    (when verbose
	      (%report-duplication ss duplicated)))
	  (if-let ((result (search-loop-path movements-shrinked ss :verbose verbose)))
	    (progn (incf total-count)
		   (push result (aref loops (1- (length ss)))))
	    (progn 
	      (when verbose
		(format t " ...failed."))
	      (collect ss into invalid-loops))))
	(finally
	 (format t "~%duplicated loops detected --- ~a/~a" duplicated-count i)
	 (format t "~%valid loops in total --- ~a/~a" total-count i)
	 ;; (format t "~%these loops were invalid:~%~w" invalid-loops)
	 
	 (return
	   (values (reduce #'append loops)
		   invalid-loops)))))
