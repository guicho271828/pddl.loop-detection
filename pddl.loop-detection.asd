#|
  This file is a part of pddl.loop-detection project.
|#

(defsystem pddl.loop-detection
  :version "0.1"
  :author "Masataro Asai"
  :license "LLGPL"
  :depends-on (:pddl :pddl.scheduler
                     :eazy-a-star
                     :trivial-timeout
                     :eazylazy
                     :inferior-shell
                     :osicat)
  :components ((:file :package :pathname "src/package")
	       (:module "src"
		:depends-on (:package)
                :serial t
                :components
                ((:file :mutex)
		 (:file :movement)
                 (:file :steady-state)
                 (:file :mfp)
		 (:file :mfp-with-filtering)
                 (:file :best-first-mfp)
		 (:file :problem-builder)
                 (:file :search))))
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
  (eval (read-from-string "(fiveam:run! 'pddl.loop-detection)"))
  t)
