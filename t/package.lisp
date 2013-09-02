#|
  This file is a part of pddl.loop-detection project.
|#

(in-package :cl-user)
(defpackage pddl.loop-detection-test
  (:use :cl
	:guicho-utilities
	:guicho-a*
	:guicho-red-black-tree
	:iterate
	:alexandria
	:optima
	:repl-utilities
        :pddl
	:pddl.scheduler
        :pddl.loop-detection
	:pddl.plan-optimizer
        :pddl.instances
        :fiveam)
  (:shadow :place :maximize :minimize)
  (:shadowing-import-from :guicho-a* :cost)
  (:shadowing-import-from :fiveam :fail))
(in-package :pddl.loop-detection-test)

(def-suite :pddl.loop-detection :in :pddl)
(in-suite :pddl.loop-detection)

(defparameter schedule
  (reschedule
     cell-assembly-model2a-1-6
     :minimum-slack))

;; 輸送型のアクションを検出できれば - 場所がわかるかも。
;; 
;; どうやら、ベースごとのアクション列は同じみたいだ。(n=2の場合。)
;; 理論的に検証できるか?
;; 
;; 今は lama-2011-opt だが、sat ではどうか?

;; 場所の列だけでのか?

(test same-actions-per-base
  (is (every (lambda (ta0 ta1)
	       (eq (name (timed-action-action ta0))
		   (name (timed-action-action ta1))))
	     (filter-schedule schedule :objects '(b-0))
	     (filter-schedule schedule :objects '(b-1)))))

(defvar movements)
(defvar movements-indices)

(multiple-value-setq
    (movements movements-indices)
  (extract-movements 'b-0 schedule cell-assembly))

(defvar movements-shrinked)
(defvar movements-indices-shrinked)

(multiple-value-setq
    (movements-shrinked movements-indices-shrinked)
  (shrink-movements movements movements-indices))

(defparameter movements-shrinked2
  (nthcdr 10 movements-shrinked))
(defparameter movements-indices-shrinked2
  (nthcdr 10 movements-indices-shrinked))

(test extract-movements
  (finishes
    (shrink-movements                ; (fact*)* --> (fact*)*
       (extract-movements               ; (object,schedule,domain) --> (fact*)*
	'b-0                           ; pddl-object/symbol
	(reschedule                    ; (plan, algorithm) --> schedule
	 cell-assembly-model2a-2-9     ; pddl-plan (model2a, 2 bases)
	 :minimum-slack)               ; (eql :minimum-slack)
	cell-assembly))
  ))

(defparameter steady-states
  (exploit-steady-states movements-shrinked))
(defparameter steady-states2
  (exploit-steady-states movements-shrinked2))

(test (steady-states :depends-on extract-movements)
  (let ((movements movements-shrinked2))
    (dolist (ss (exploit-steady-states movements))
      (when (>= (length ss) 2)
	(map-combinations
	 (lambda (list)
	   (is (null (intersection (first list) (second list)))))
	 (mapcar (rcurry #'nth movements) ss)
	 :length 2))
      (is (= 0 (first ss))))))

(test (search-loop-path)
  (time (search-loop-path movements-shrinked
			  (lastcar steady-states))))

(defun test-interactively ()
  (let ((i 0))
    (do-restart ((next (lambda ()
			 (incf i))))
      (print (nth i steady-states))
      (let ((path (time (search-loop-path movements-shrinked (nth i steady-states)))))
	(print path)
	(terpri))
      (error "what to do next?"))))

(defparameter loopable-steady-states
  (loopable-steady-states movements-shrinked steady-states :verbose nil))

(test (loopable-steady-states :depends-on search-loop-path)
  (time (loopable-steady-states movements-shrinked2)))

(defparameter loopable-steady-states-sorted
  (sort-loops loopable-steady-states
	      schedule
	      movements-indices))

(defparameter loopable-steady-states-sorted-score-distribution
  (mapcar (curry #'loop-heuristic-cost
		 schedule movements-indices)
	  loopable-steady-states))

(defparameter loop-plan
  (random-elt loopable-steady-states))
(defparameter base-type
  (type (object cell-assembly-model2a-1 'b-0)))

(defparameter prob
  cell-assembly-model2a-1)

(defparameter steady-state-problem
  (build-steady-state-problem
   prob loop-plan schedule
   movements-shrinked movements-indices-shrinked base-type))

(defparameter steady-state-problems
  (iter (for loop-plan in loopable-steady-states)
	(collect
	    (build-steady-state-problem
	     prob loop-plan schedule
	     movements-shrinked movements-indices-shrinked base-type))))