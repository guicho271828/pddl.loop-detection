#|
  This file is a part of pddl.loop-detection project.
|#

(in-package :cl-user)
(defpackage pddl.loop-detection
  (:use :cl
	:guicho-utilities
	:guicho-a*
	:iterate
	:optima
	:annot.doc
	:cl-syntax
        :pddl
	:pddl.scheduler
	:pddl.plan-optimizer
	:alexandria)
  (:shadowing-import-from
   :guicho-a* :cost)            ;resolve conflict with pddl:cost
  (:shadowing-import-from
   :iterate :minimize :maximize))
(in-package :pddl.loop-detection)

;; blah blah blah.


