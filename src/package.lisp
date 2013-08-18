#|
  This file is a part of pddl.loop-detection project.
|#

(in-package :cl-user)
(defpackage pddl.loop-detection
  (:use :cl
	:guicho-utilities
	:iterate
	:optima
	:cl-syntax
        :pddl
	:pddl.scheduler
	:alexandria)
  (:shadowing-import-from
   :iterate :minimize :maximize))
(in-package :pddl.loop-detection)

;; blah blah blah.


