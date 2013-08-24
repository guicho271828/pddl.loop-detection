(in-package :pddl.loop-detection)
(use-syntax :annot)

(defun movement-transition-cost (schedule movement1 movement2)
  (iter (with start-time = nil)
	(for ta in schedule)
	(match ta
	  ((timed-action
	    (end (timed-state time state)))
	   (cond
	     ((null start-time)
	      (when (subsetp movement1 state)
		(setf start-time time)))
	     (t
	      (when (subsetp movement2 state)
		(return-from movement-transition-cost
		  (- time start-time)))))))))


(defun loop-heuristic-cost (schedule movements loop-plan)
  (iter (for ta in schedule)
	(member (timed-state-state
	 (timed-action-end ta))

(mapcar (compose #'timed-state-state
		 #'timed-action-end) schedule)
