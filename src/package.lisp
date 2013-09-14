#|
  This file is a part of pddl.loop-detection project.
|#

(in-package :cl-user)
(defpackage pddl.loop-detection
  (:use :cl
        :guicho-utilities
        :guicho-utilities.threading
        :guicho-a*
        :guicho-red-black-tree
        :iterate
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
  (:shadowing-import-from
   :guicho-a* :cost)            ;resolve conflict with pddl:cost
  (:shadowing-import-from
   :iterate :minimize :maximize))
(in-package :pddl.loop-detection)

;; blah blah blah.


