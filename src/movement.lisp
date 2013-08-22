
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

