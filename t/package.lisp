#|
  This file is a part of pddl.loop-detection project.
|#

(in-package :cl-user)
(defpackage pddl.loop-detection-test
  (:use :cl
	:guicho-utilities
	:iterate
	:alexandria
	:optima
	:optima
        :pddl
	:pddl.scheduler
        :pddl.loop-detection
        :pddl-test
        :fiveam)
  (:shadow :place :maximize :minimize)
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


