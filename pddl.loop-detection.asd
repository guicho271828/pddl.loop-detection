#|
  This file is a part of pddl.loop-detection project.
|#

(in-package :cl-user)
(defpackage pddl.loop-detection-asd
  (:use :cl :asdf))
(in-package :pddl.loop-detection-asd)

(defsystem pddl.loop-detection
  :version "0.1"
  :author "Masataro Asai"
  :license "LLGPL"
  :depends-on (:pddl :pddl.scheduler
                     :guicho-utilities.threading
                     :eazy-a-star
                     :eazylazy
                     :inferior-shell
                     :osicat
                     :bordeaux-threads
                     :lparallel)
  :components ((:file :package :pathname "src/package")
	       (:module "src"
		:depends-on (:package)
                :serial t
                :components
                ((:file :iterator)
                 (:file :mutex)
		 (:file :movement)
		 (:file :steady-state)
                 (:file :steady-state-tree)
		 (:file :simulator)
		 (:file :simulator-with-tree)
		 (:file :ranker)
		 (:file :problem-builder)
                 (:file :write-problem)
                 (:file :integrated))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.org"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op pddl.loop-detection-test))))

(defmethod asdf:perform ((op asdf:test-op)
			 (system (eql (asdf:find-system :pddl.loop-detection))))
  (funcall (find-symbol "RUN!" (find-package :fiveam)) :pddl.loop-detection)
  t)
