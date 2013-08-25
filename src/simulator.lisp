(in-package :pddl.loop-detection)
(use-syntax :annot)

@export
(defclass state-node (searchable-node)
  ((current-state :initarg :current-state :reader current-state)
   (movements :initarg :movements :reader movements)
   (complementary-edge-class :initform 'transition)))

(defvar *movements-hash*
  (make-hash-table :test #'equalp))
(defmethod allocate-instance :around
    ((class (eql (find-class 'state-node)))
     &key movements current-state)
  (let ((state-hash
	 (or (gethash movements *movements-hash*)
	     (setf (gethash movements *movements-hash*)
		   (make-hash-table :test #'equalp)))))
    ;; (when (gethash current-state state-hash)
    ;;   (format t "allocation stopped!"))
    (or (gethash current-state state-hash)
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

@export
(defun search-loop-path (movements steady-state)
  (handler-return ((path-not-found (lambda (c)
				     @ignore c
				     nil)))
    (let ((last (a*-search
		 (make-instance
		  'state-node
		  :movements movements
		  :current-state steady-state)
		 (make-instance
		  'state-node
		  :movements movements
		  :current-state (make-eol steady-state
					   (length movements))))))
      (iter (for node first last then (parent node))
	    (while node)
	    (collect (current-state node) at beginning)))))

(defun %make-state-node (movements current-state n)
  (make-instance
   'state-node
   :movements movements
   :current-state (substitute (1+ n) n current-state)))

(defmethod generate-nodes ((state state-node))
  (with-slots (movements current-state) state
    ;; movements: (mutex*)*
    ;; current: number*
    (let ((max (length movements))
	  (used (mappend (rcurry #'nth movements) current-state)))
      ;; used: mutices currently in use
      (mapcar
       (curry #'%make-state-node movements current-state)
       (remove-if-not
	(lambda (n)
	  (let ((n2 (1+ n)))
	    (and
	     ;; n=max iff n is carry-out. 
	     (< n max)
	     ;; carry-out is ignored
	     (not (member n2 current-state))
	     ;; if the next place is carry-out, it does not
	     ;; consume any resource
	     (if (= max n2)
		 t
		 (null
		  (intersection
		   (nth n2 movements)
		   (set-difference used (nth n movements))))))))
	current-state)))))

(defun %report-duplication (ss duplicated)
  @ignore duplicated
  (format t "~%~w is not searched because it had appeared in the other loop." ss))

(defun %check-duplicate (ss loops)

  (some
   (lambda (path)
     (member ss path :test #'equalp))
   (aref loops (length ss))))

@export
(defun loopable-steady-states (movements)
  "Returns the list of solution path from start-of-loop to end-of-loop.
start-of-loop is always the same list to the steady-state in the
meaning of EQUALP."
  (iter (with loops = (make-array (length movements) :initial-element nil))
	(with steady-states = (exploit-steady-state movements))
	(with max = (length steady-states))
	(for i from 0)
	(for ss in steady-states)
	(format t "~%~a/~a: " i max)
	(if-let ((duplicated (%check-duplicate ss loops)))
	  (%report-duplication ss duplicated)
	  (when-let ((result (search-loop-path movements ss)))
	    (push result (aref loops (1- (length ss))))))
	(finally (return loops))))
