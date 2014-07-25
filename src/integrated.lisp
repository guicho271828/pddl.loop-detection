
(in-package :pddl.loop-detection)
(cl-syntax:use-syntax :annot)


(declaim (ftype (function
                 (pddl-plan t (function (pddl-problem list list list list)
                                        real)
                            &key (:verbose boolean))
                 (values list list real))
                exploit-loop-problems))

@export
(defun exploit-loop-problems (pddl-plan component evaluator &key verbose)
  (let* ((*problem* (problem pddl-plan))
         (*domain* (domain pddl-plan))
         (component (ensure-list component))
         (schedule (sort-schedule
                    (reschedule pddl-plan :minimum-slack
                                :verbose verbose)))
         (movements
          (iter (for c in component)
                (reducing
                 (extract-movements c schedule)
                 by #'merge-movements))))
    (when verbose
      (format t
              "~&Exploiting loopable steady-states from the movements.
Please wait a moment..."))
    (iter (for (values plan ss handler)
               initially (best-first-mfp movements :verbose verbose)
               then (funcall handler real-cost))
          (unless plan (finish))
          (for real-cost = (funcall evaluator
                                    *problem*
                                    ss
                                    schedule
                                    movements
                                    component))
          (finding plan minimizing real-cost into best-plan)
          (finding ss minimizing real-cost into best-ss)
          (finding real-cost minimizing real-cost into best-cost)
          (finally
           (return (values best-plan
                           best-ss
                           best-cost))))))


