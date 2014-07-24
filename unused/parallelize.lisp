
;; not used now

(in-package :pddl.loop-detection)
(use-syntax :annot)


(defun eff+ (prev next)
  (set-difference next prev))
(defun eff- (prev next)
  (set-difference prev next))
(defun precond (prev next)
  (set-difference prev next))


@export
(defun parallelize-loop-plan (loop-plan movements-shrinked)
  (flet ((mutices (state) (mappend (rcurry #'nth movements-shrinked) state)))
    (list* (first loop-plan)
           (iter (with pstate-n = (car loop-plan))
                 (with used-mutices = (mutices pstate-n))
                 (for state in (cdr loop-plan))
                 (for pstate-1 previous state)


                 ))))


