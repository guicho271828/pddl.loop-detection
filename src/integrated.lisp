
(in-package :pddl.loop-detection)
(cl-syntax:use-syntax :annot)

(defun message ()
  (format *standard-output*
          "~2&Exploiting loopable steady-states from the movements.
Please wait a moment...~%"))

@export
(defun exploit-loop-problems (unit-plan base-object &key verbose)
  (terpri *standard-output*)
  (pprint-logical-block (*standard-output*
                         nil
                         :per-line-prefix "; Preparation process: ")
    (let* ((*problem* (problem unit-plan))
           (*domain* (domain unit-plan))
           (tmpdir (mktemp :pddl))
           (base-type (type (object *problem* base-object))))
      (let ((schedule (sort-timed-actions
                       (reschedule unit-plan
                                   :minimum-slack
                                   :verbose nil))))
        (multiple-value-bind (movements movements-indices)
            (extract-movements base-object schedule *domain*)
          (message)
          (values
           (mapcar (lambda (loop-plan)
                     (list (write-problem
                            (build-steady-state-problem
                             *problem*
                             loop-plan
                             schedule
                             movements
                             movements-indices
                             base-type)
                            tmpdir)
                           (caar loop-plan)))
                   (time (exploit-loopable-steady-states
                          movements
                          (exploit-steady-states movements)
                          :verbose verbose)))
           base-type))))))


