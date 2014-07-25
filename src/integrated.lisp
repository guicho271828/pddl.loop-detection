
(in-package :pddl.loop-detection)
(cl-syntax:use-syntax :annot)

(defun message ()
  (format *standard-output*
          "~2&Exploiting loopable steady-states from the movements.
Please wait a moment...~%"))

(defun exploit-loop-problems (unit-plan component &key verbose)
  (terpri *standard-output*)
  (pprint-logical-block (*standard-output*
                         nil
                         :per-line-prefix "; Preparation process: ")
    (let* ((*problem* (problem unit-plan))
           (*domain* (domain unit-plan))
           (component (ensure-list component))
           (tmpdir (mktemp :pddl))
           (schedule (sort-schedule
                      (reschedule unit-plan :minimum-slack
                                  :verbose verbose)))
           (movements
            (iter (for c in component)
                  (reducing
                   (extract-movements c schedule *domain*)
                   by #'merge-movements))))
      (message)
      (iter (for (values plan ss handler)
                 initially (best-first-mfp movements :verbose verbose)
                 then (funcall handler 0))
            (unless (and handler ss)
              (finish))
            (write-problem
             (apply #'loop-problem
                    *problem* ss schedule movements
                    component)
             tmpdir)))))


