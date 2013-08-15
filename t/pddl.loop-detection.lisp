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
  (:shadow :place)
  (:shadowing-import-from :fiveam :fail))
(in-package :pddl.loop-detection-test)

(def-suite :pddl.loop-detection :in :pddl)
(in-suite :pddl.loop-detection)

