
(in-package :pddl.loop-detection)
(cl-syntax:use-syntax :annot)


(declaim (ftype (function
                 (pddl-plan t (function (pddl-problem
                                         list list list list
                                         &key (:verbose boolean))
                                        real)
                            &key
                            (:verbose boolean)
                            (:timeout fixnum))
                 (values list list real))
                exploit-loop-problems))

@export
(defun exploit-loop-problems (pddl-plan component evaluator
                              &key verbose (timeout MOST-POSITIVE-FIXNUM))
  "Returns 3 values: loop-path, ss, the real-cost of the loop path returned
by the evaluator."
  (when verbose
    (format t "~&Running MFP ..."))
  (let (best-plan best-ss best-cost)
    (handler-case
      (with-timeout (timeout)
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
          (iter (for (values plan ss handler)
                     initially (best-first-mfp movements :verbose verbose)
                     then (funcall handler cost))
                (unless plan (finish))
                (for cost = (funcall evaluator
                                     *problem*
                                     ss
                                     schedule
                                     movements
                                     component
                                     :verbose verbose))
                (when (or (null best-cost) (< cost best-cost))
                  (setf best-plan plan
                        best-ss ss
                        best-cost cost)))))
      (timeout-error (c)
        (write c)))
    (values best-plan best-ss best-cost)))


