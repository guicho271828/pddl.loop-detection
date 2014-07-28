#|
  This file is a part of pddl.loop-detection project.
|#

(in-package :cl-user)
(defpackage pddl.loop-detection
  (:use :cl
        :alexandria
        :bordeaux-threads
        :cl-syntax
        :cl-rlimit
        :eazy-a-star
        :eazylazy
        :guicho-red-black-tree
        :guicho-utilities
        :iterate
        :optima
        :pddl
        :pddl.scheduler)
  ;; resolve conflict with pddl:cost
  (:shadowing-import-from :eazy-a* :cost)
  (:shadow :minimize :maximize))
(in-package :pddl.loop-detection)

;; blah blah blah.


