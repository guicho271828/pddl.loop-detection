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
	       :pddl.instances
               :eazylazy
               :repl-utilities
               :fiveam)
  :components ((:module "t"
                :components
                ((:file :package)
                 (:file :utility)
                 ;; domain functions
                 (:file :mutex)
                 ;; problem functions
                 (:file :basis)
                 (:file :movements)
                 (:file :test1))))
  :perform (load-op :after (op c) 
		    (eval (read-from-string "(fiveam:run! :pddl.loop-detection)"))
		    (asdf:clear-system c)))
