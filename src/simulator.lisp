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
      (format s "( ~{~3@<~a ~>~})" current-state))))


@export
(defclass transition (searchable-edge)
  ((complementary-node-class :initform 'state-node)))

(defmethod generic-eq ((n1 state-node) (n2 state-node))
  (equalp (current-state n1)
	  (current-state n2)))

(defmethod cost ((tr transition))
  1)
(defmethod heuristic-cost-between ((n1 state-node) (n2 state-node))
  (iter (for pos1 in (current-state n1))
	(for pos2 in (current-state n2))
	(summing (abs (- pos2 pos1)))))

(defmethod constraint-ordering-op ((n state-node))
  (heuristic-cost-between n (goal n)))

@export
(defun search-loop-path (movements steady-state)
  (remhash movements *movements-hash*)
  (handler-return ((path-not-found (lambda (c)
				     @ignore c
				     nil)))
    (let* ((goal (make-instance
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
      (let ((last (a*-search start goal)))
	(iter (for node first last then (parent node))
	      (while node)
	      (collect (current-state node) at beginning))))))

(defun %make-state-node (movements current-state goal n)
  (make-instance
   'state-node
   :goal goal
   :movements movements
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
		    (not (= n goal-n)) ; n is not achieved yet
		    (< n max) ; n+1 is not carry-out.
		    (not (member n2 current-state)) ; n+1 is unoccupied
		    (if (= max n2) ; if n+1 is carry-out
			t          ; always ok, no consumption of resource
			(null      ; else, the resource constraint should be kept
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
(defun loopable-steady-states (movements)
  "Returns the list of solution path from start-of-loop to end-of-loop.
start-of-loop is always the same list to the steady-state in the
meaning of EQUALP."

  (iter (with loops = (make-array (length movements) :initial-element nil))
	(with steady-states = (exploit-steady-state movements))
	(with max = (length steady-states))
	(with duplicated-count = 0)
	(with total-count = 0)
	(for i from 0)
	(for ss in steady-states)
	(format t "~%~a/~a: " i max)
	(if-let ((duplicated (%check-duplicate ss loops)))
	  (progn
	    (incf duplicated-count)
	    (incf total-count)
	    (%report-duplication ss duplicated))
	  (if-let ((result (search-loop-path movements ss)))
	    (progn (incf total-count)
		   (push result (aref loops (1- (length ss)))))
	    (collect ss into invalid-loops)))
	(finally
	 (format t "~%duplicated loops detected --- ~a/~a" duplicated-count i)
	 (format t "~%valid loops in total --- ~a/~a" total-count i)
	 (format t "~%these loops were invalid:~%~w" invalid-loops)
	 
	 (return (values loops invalid-loops)))))
