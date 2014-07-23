#|
  This file is a part of pddl.loop-detection project.
|#

(in-package :cl-user)
(defpackage pddl.loop-detection
  (:use :cl
        :guicho-utilities
        :eazy-a*
        :guicho-red-black-tree
        :iterate
        :optima
        :osicat
        :annot.doc
        :cl-syntax
        :pddl
        :pddl.scheduler
        :pddl.plan-optimizer
        :alexandria)
  ;; resolve conflict with pddl:cost
  (:shadowing-import-from :eazy-a* :cost)
  (:shadow :minimize :maximize))
(in-package :pddl.loop-detection)

;; blah blah blah.


