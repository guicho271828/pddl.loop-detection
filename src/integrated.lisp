
(in-package :pddl.loop-detection)
(cl-syntax:use-syntax :annot)

(defun message ()
  (format *standard-output*
          "~2&Exploiting loopable steady-states from the movements.
Please wait a moment...~%"))

(defun mktemp ()
  (iter (for path = (merge-pathnames
                     (format nil "pddl.tmp.~x"
                             (random MOST-POSITIVE-FIXNUM))
                     #p"/tmp/"))
        (unless (directory-exists-p path)
          (ensure-directories-exist path :verbose t)
          (return-from mktemp path))))

@export
(defun exploit-loop-problems (unit-plan base-object)
  (terpri *standard-output*)
  (pprint-logical-block (*standard-output*
                         nil
                         :per-line-prefix "; Preparation process: ")
    (let* ((*problem* (problem unit-plan))
           (*domain* (domain unit-plan))
           (tmpdir (mktemp))
           (base-type (type (object *problem* base-object))))
      (let ((schedule (reschedule unit-plan
                                  :minimum-slack
                                  :verbose nil)))
        (multiple-value-bind (movements movements-indices)
            (extract-movements base-object schedule *domain*)
          (message)
          (return-from exploit-loop-problems
            (values
             (mapcar (lambda (loop-plan)
                       (write-problem
                        (build-steady-state-problem
                         *problem*
                         loop-plan
                         schedule
                         movements
                         movements-indices
                         base-type)
                        tmpdir))
                     (time (exploit-loopable-steady-states
                            movements
                            (exploit-steady-states movements)
                            :verbose nil)))
             base-type)))))))