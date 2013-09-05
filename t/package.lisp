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
(defvar movements-shrinked)
(defvar movements-indices-shrinked)

(test extract-movements
  (finishes
    (multiple-value-setq
	(movements movements-indices)
      (extract-movements 'b-0 schedule cell-assembly))
    
    (multiple-value-setq
	(movements-shrinked movements-indices-shrinked)
      (shrink-movements movements movements-indices))))

(defvar steady-states)

(test (steady-states)
  (finishes
    (setf steady-states 
	  (exploit-steady-states movements-shrinked)))
  (let ((*num-trials* 30))
    (for-all ((ss (lambda () (random-elt steady-states))))
      (when (>= (length ss) 2)
	(map-combinations
	 (lambda (list)
	   (is (null (intersection (first list) (second list)))))
	 (mapcar (rcurry #'nth movements-shrinked) ss)
	 :length 2))
      (is (= 0 (first ss))))))

(test (search-loop-path)
  (time (search-loop-path
	 movements-shrinked
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

(defvar loopable-steady-states)
(test (loopable-steady-states)
  (format t "~%testing loopable-steady-states. It takes time so please wait...~2%")
  (finishes
    (time (setf loopable-steady-states
		(loopable-steady-states
		   movements-shrinked
		   steady-states :verbose nil)))))

(defparameter prob
  cell-assembly-model2a-1)
(defparameter base-type
  (type (object prob 'b-0)))

(defvar steady-state-problems)
(defvar steady-state-problem)

(test (build-problem)
  (finishes
    (time
     (setf steady-state-problems
	   (iter (for loop-plan in loopable-steady-states)
		 (collect
		     (build-steady-state-problem
		      prob loop-plan schedule
		      movements-shrinked movements-indices-shrinked base-type))))))

  (for-all ((problem1 (lambda () (random-elt steady-state-problems)))
	    (problem2 (lambda () (random-elt steady-state-problems))))
    (unless (eq problem1 problem2)
      (is-false (equal (goal problem1) (goal problem2)))))
  
  (for-all ((problem1 (lambda () (random-elt steady-state-problems))))

    (is-true (some (lambda (obj)
		     (search "BASE0" (symbol-name (name obj))))
		   (objects/const problem1))))
  

  (setf steady-state-problem
	(random-elt steady-state-problems)))
