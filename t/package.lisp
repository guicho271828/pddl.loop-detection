#|
This file is a part of pddl.loop-detection project.
|#

(in-package :cl-user)
(defpackage pddl.loop-detection-test
  (:use :cl
	:guicho-utilities
	:eazy-a*
	:guicho-red-black-tree
	:iterate
	:alexandria
	:optima
	:repl-utilities
        :eazylazy
        :pddl
	:pddl.scheduler
        :pddl.loop-detection
	:pddl.plan-optimizer
        :pddl.instances
        :fiveam)
  (:shadow :place :maximize :minimize)
  (:shadowing-import-from :eazy-a* :cost)
  (:shadowing-import-from :fiveam :fail))
(in-package :pddl.loop-detection-test)

(def-suite :pddl.loop-detection)
(in-suite :pddl.loop-detection)
