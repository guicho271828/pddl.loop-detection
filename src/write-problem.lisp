
(in-package :pddl.loop-detection)
(use-syntax :annot)


@export
(defun write-problem (problem
                      &optional
                        (basedir (user-homedir-pathname))
                        verbose)
  (let ((path 
         (merge-pathnames
          (let ((*print-escape* nil))
            (format nil "~a/~a.pddl"
                    (name (domain problem))
                    (name problem)))
          (pathname-as-directory basedir))))
    (ensure-directories-exist path :verbose verbose)
    (when verbose
      (format t "~&Writing ~a~&" path))
    (with-open-file (s path
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (let ((*package* (find-package :pddl)))
        (print-pddl-object problem s)))
    path))

(export 'write-pddl)
(export 'write-plan)

(eval-when (:load-toplevel :execute)
  (setf (fdefinition 'write-pddl)
        (fdefinition 'write-problem))
  (setf (fdefinition 'write-plan)
        (fdefinition 'write-problem)))
