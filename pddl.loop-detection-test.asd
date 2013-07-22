#|
  This file is a part of pddl.loop-detection project.
|#

(in-package :cl-user)
(defpackage pddl.loop-detection-test-asd
  (:use :cl :asdf))
(in-package :pddl.loop-detection-test-asd)

(defsystem pddl.loop-detection-test
  :author ""
  :license ""
  :depends-on (:pddl.loop-detection
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "pddl.loop-detection"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
