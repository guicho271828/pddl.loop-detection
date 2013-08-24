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
  0)

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
		  :current-state (make-eol steady-state)))))
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
	  (and (< n max)
	       (null
		(intersection
		 (if (= max (1+ n))
		     nil
		     (nth (1+ n) movements))
		 (set-difference used (nth n movements))))))
	current-state)))))

(mapcar (curry #'search-loop-path movements2) steady-state2)