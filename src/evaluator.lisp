
(in-package :pddl.loop-planner)
(use-syntax :annot)

@export
(defclass fd-evaluation-result (evaluation-result pathnamable) ())

@export
(defun evaluate-loop-problem (problem ss schedule movements component
                              &key verbose)
  "Evaluates the real cost of a MFP path of a steady state. Meant to be
called by exploit-loop-problems."
  (let* ((*domain* (domain problem))
         (*problem*
          (apply #'loop-problem problem ss schedule movements component))
         (tmpdir
          (mktemp (name *problem*)))
         (domain-path
          (write-pddl *domain* "domain.pddl" tmpdir verbose))
         (problem-path
          (write-pddl *problem* "problem.pddl" tmpdir verbose))
         (plan-path-list
          (test-problem problem-path domain-path :verbose verbose)))
    (reduce #'evaluation-result-min
            (mapcar (lambda (path)
                      (make-instance
                       'fd-evaluation-result
                       :path path
                       :ss ss
                       :cost
                       (timed-state-time 
                        (timed-action-end
                         (lastcar
                          (sort-schedule
                           (reschedule (pddl-plan :path path)
                                       :minimum-slack)))))))
                    plan-path-list)
            :initial-value (funcall (constant-results MOST-POSITIVE-FIXNUM)
                                    *problem* ss))))

