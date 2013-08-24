#|
  This file is a part of pddl.loop-detection project.
|#

(in-package :cl-user)
(defpackage pddl.loop-detection-test
  (:use :cl
	:guicho-utilities
	:guicho-a*
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
  (reschedule cell-assembly-model2a-2-9 :minimum-slack))

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

(defvar movements
  (shrink-movements                ; (fact*)* --> (fact*)*
   (extract-movements               ; (object,schedule,domain) --> (fact*)*
    'b-0                           ; pddl-object/symbol
    (reschedule                    ; (plan, algorithm) --> schedule
     cell-assembly-model2a-2-9     ; pddl-plan (model2a, 2 bases)
     :minimum-slack)               ; (eql :minimum-slack)
    cell-assembly)))                    ; pddl-domain

(defvar movements2
  (nthcdr 15 movements))

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

(defvar steady-states
  (exploit-steady-state movements))
(defvar steady-states2
  (exploit-steady-state movements2))

(test (steady-states :depends-on extract-movements)
  (let ((movements (nthcdr 10 movements)))
    (dolist (ss (exploit-steady-state movements))
      (when (>= (length ss) 2)
	(map-combinations
	 (lambda (list)
	   (is (null (intersection (first list) (second list)))))
	 (mapcar (rcurry #'nth movements) ss)
	 :length 2)))))

