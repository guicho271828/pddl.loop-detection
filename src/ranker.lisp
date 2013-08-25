(in-package :pddl.loop-detection)
(use-syntax :annot)

@export
(defun movement-transition-cost (schedule index1 index2)
  (- (timed-state-time (timed-action-start (nth index2 schedule)))
     (timed-state-time (timed-action-start (nth index1 schedule)))))

;; loops is (list plan*)

@export
(defun loop-heuristic-cost (schedule movements-indices loop-plan)
  "SCHEDULE must be a fundamental-schedule (which only processes ONE base).
DO NOT use shrinked MOVEMENTS-INDICES. It will cause an error."
  (iter (for state in loop-plan)
	(for pstate previous state)
	(unless pstate (next-iteration))
	(for diff = (set-difference state pstate))
	(assert (= 1 (length diff)))
	(for changed = (car diff))
	(assert (plusp changed))
	(summing
	 (movement-transition-cost
	  schedule
	  (nth (1- changed) movements-indices)
	  (nth changed movements-indices)))))

@export
(defun sort-loops (loops schedule movements-indices)
  (sort loops #'< :key (curry #'loop-heuristic-cost
			      schedule movements-indices)))
  
