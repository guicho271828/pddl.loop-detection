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
  :depends-on (:pddl.loop-detection :fiveam)
  :components ((:module "t"
                :components
                ((:file :package)
                 (:file :utility)
                 ;; domain functions
                 (:file :mutex)
                 ;; problem functions
                 (:file :basis)
                 (:file :basis-component)
                 (:file :movements)
                 (:file :search)
                 (:file :build-problem)
                 (:file :exploit-loop-problems))))
  :perform (load-op :after (op c) 
		    (eval (read-from-string "(fiveam:run! :pddl.loop-detection)"))
		    (asdf:clear-system c)))
