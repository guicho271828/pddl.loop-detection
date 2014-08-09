#|
This file is a part of pddl.loop-detection project.
|#

(in-package :cl-user)
(defpackage pddl.loop-detection-test
  (:use :cl
	:alexandria
        :bordeaux-threads
        :eazy-a*
        :eazylazy
        :fiveam
        :guicho-red-black-tree
        :guicho-utilities
        :iterate
        :optima
        :pddl
        :pddl.loop-detection
        :pddl.scheduler)
  (:shadow :place)
  (:shadowing-import-from :pddl :maximize :minimize :cost)
  (:shadowing-import-from :fiveam :fail))
(in-package :pddl.loop-detection-test)

(def-suite :pddl.loop-detection)
(in-suite :pddl.loop-detection)
