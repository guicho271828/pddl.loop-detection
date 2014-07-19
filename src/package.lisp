#|
  This file is a part of pddl.loop-detection project.
|#

(in-package :cl-user)
(defpackage pddl.loop-detection
  (:use :cl
        :guicho-utilities
        :guicho-utilities.threading
        :eazy-a*
        :guicho-red-black-tree
        :iterate
        :eazylazy
        :optima
        :osicat
        :annot.doc
        :cl-syntax
        :pddl
        :pddl.scheduler
        :pddl.plan-optimizer
        :alexandria
        :bordeaux-threads
        :lparallel)
  (:shadow :force :delay)
  (:shadowing-import-from
   :eazy-a* :cost)            ;resolve conflict with pddl:cost
  (:shadowing-import-from
   :iterate :minimize :maximize))
(in-package :pddl.loop-detection)

;; blah blah blah.


