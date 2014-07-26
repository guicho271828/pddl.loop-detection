#|
  This file is a part of pddl.loop-detection project.
|#

(in-package :cl-user)
(defpackage pddl.loop-detection
  (:use :cl
        :alexandria
        :annot.doc
        :cl-syntax
        :eazy-a*
        :eazylazy
        :guicho-red-black-tree
        :guicho-utilities
        :iterate
        :optima
        :osicat
        :pddl
        :pddl.plan-optimizer
        :pddl.scheduler
        :trivial-timeout)
  ;; resolve conflict with pddl:cost
  (:shadowing-import-from :eazy-a* :cost)
  (:shadow :minimize :maximize))
(in-package :pddl.loop-detection)

;; blah blah blah.


