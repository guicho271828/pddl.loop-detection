
(in-package :pddl.loop-detection)
(use-syntax :annot)

@export
(defun related-actions (predicate)
  "returns a list of actions which uses the specified predicate in
its precondition or the effect."
  (match predicate
    ((pddl-predicate name domain)
     (remove-if-not
      (named-lambda per-action (action)
	(flet ((per-branch (branch cont)
		 (match branch
		   ((op _ preds) (funcall cont preds))
		   ((pddl-predicate :name name2)
		    (when (eq name name2)
		      (return-from per-action t))))))
	  (walk-tree #'per-branch (precondition action))
	  (walk-tree #'per-branch (effect action))
	  nil))
      (actions domain)))))

(defmethod related-to (designator (ta timed-action))
  (related-to designator (timed-action-action ta)))

@export
(defun acquire-or-release-mutex-p (mutex action)
  (match mutex
    ((list* owner _)
     (or (some (curry #'%matches-to-owner-p owner)
	       (add-list action))
	 (some (curry #'%matches-to-owner-p owner)
	       (delete-list action))))))

@export
(defun extract-movements (object schedule domain)
  (mapcar (compose
	   (let ((owners (mapcar #'first (mutex-predicates domain))))
	     (lambda (states)
	       (remove-if-not
	   	(lambda (atomic-state)
		  (some
		   (rcurry #'%matches-to-owner-p atomic-state)
		   owners))
	   	states)))
	   (lambda (ta)
	     (match ta
	       ((timed-action
		 :end (timed-state :state states))
		(remove-if-not (curry #'related-to object)
			       states)))))
	  (remove-if-not (curry #'related-to object)
			 schedule)))

@export
(defun shrink-movements (movements)
  (iter (for states in movements)
	(for pstates previous states)
	(unless pstates
	  (next-iteration))
	(unless (or (set-equal states pstates)
		    (null states))
	  (collect states))))
