#|
  This file is a part of pddl.loop-detection project.
|#

(in-package :cl-user)
(defpackage pddl.loop-detection-test
  (:use :cl
	:guicho-utilities
	:iterate
	:optima
        :pddl.loop-detection
        :pddl
        :fiveam)
  (:shadow :place)
  (:shadowing-import-from :fiveam :fail))
(in-package :pddl.loop-detection-test)

(def-suite :pddl.loop-detection :in :pddl)
(in-suite :pddl.loop-detection)

(defun data (name)
  (merge-pathnames
   name
   (asdf:system-relative-pathname
    :pddl.loop-detection "data/")))

(defvar +domain+ (data "domain.pddl"))
(defvar +problem+ (data "p1.pddl"))
(defvar +plan+ (data "p1.plan.1"))

(print +problem+)
(print +domain+)
(print +plan+)

(test parse-domain
  (finishes (parse-file +domain+)))

(test parse-problem
  (finishes (parse-file +problem+)))

