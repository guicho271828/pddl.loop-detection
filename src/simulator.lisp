(in-package :pddl.loop-detection)
(use-syntax :annot)

@export
(defclass state-node (searchable-node)
  ((current-state :initarg :current-state :reader current-state)
   (movements :initarg :movements :reader movements)
   (complementary-edge-class :initform 'transition)))


(defmethod print-object ((n state-node) s)
  (print-unreadable-object (n s :type t)
    (with-slots (current-state) n
      (format s "~w" current-state))))


@export
(defclass transition (searchable-edge)
  ((complementary-node-class :initform 'state-node)))

(defmethod generic-eq ((n1 state-node) (n2 state-node))
  (equalp (current-state n1)
	  (current-state n2)))

(defmethod cost ((tr transition))
  1)
(defmethod heuristic-cost-between ((n1 state-node) (n2 state-node))
  (- (length (current-state n1))
     (iter (for pos1 in (current-state n1))
	   (for pos2 in (current-state n2))
	   (counting (= pos1 pos2)))))

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


(defmethod generate-nodes ((state state-node))
  (with-slots (movements current-state) state
    
    ;; movements: (mutex*)*
    ;; current: number*
    (let ((max (length movements))
	  (used (mappend (rcurry #'nth movements) current-state)))
      ;; used: mutices currently in use
      (mapcar
       (lambda (n)
	 (make-instance
	  'state-node
	  :movements movements
	  :current-state (substitute (1+ n) n current-state)))
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

@export
(defun loopable-steady-states (movements)
  "Returns the list of solution path from start-of-loop to end-of-loop.
start-of-loop is always the same list to the steady-state in the
meaning of EQUALP."

  (iter (for ss in (exploit-steady-state movements))
	(if-let ((duplicated (some
			      (lambda (path)
				(member ss path))
			      loops)))
	  (format t "~w is not searched because
it had appeard in the transitional states in the other loop scheme ~w."
		  ss duplicated)
	  (when-let ((result (search-loop-path movements ss)))
	    (collect result into loops)))
	(finally (return loops))))
