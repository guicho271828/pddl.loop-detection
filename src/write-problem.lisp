
(in-package :pddl.loop-detection)
(use-syntax :annot)


@export
(defun write-problem (problem
                      &optional (basedir
                                 (user-homedir-pathname)))
  (let ((path 
         (merge-pathnames
          (let ((*print-escape* nil))
            (format nil "~a/~a.pddl"
                    (name (domain problem))
                    (name problem)))
          (pathname-as-directory basedir))))
    (ensure-directories-exist path :verbose t)
    (format t "~&Writing ~a~&" path)
    (with-open-file (s path
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (let ((*package* (find-package :pddl)))
        (print-pddl-object problem s)))
    path))
