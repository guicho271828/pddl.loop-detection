
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
  (let (related indices)
    (multiple-value-setq
	(related indices)
      (iter (for ta in schedule)
	    (for i from 0)
	    (when (related-to object ta)
	      (collect ta into related)
	      (collect i into indices))
	    (finally (return (values related indices)))))

    (values
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
	     related)
     indices)))

@export
(defun shrink-movements (movements indices)
  (iter (for states in movements)
	(for i in indices)
	(for pstates previous states)
	(for previ previous i)
	(unless pstates
	  (next-iteration))
	(unless (or (set-equal states pstates :test #'eqstate)
		    (null states))
	  (collect states into shrinked-states)
	  (collect previ into shrinked-state-indices))
	(finally
	 (return (values
		  shrinked-states
		  shrinked-state-indices)))))
