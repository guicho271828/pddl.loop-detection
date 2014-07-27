
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

(define-local-function %exploit-main ()
  (let* ((*standard-output* s)
         (*error-output* e)
         (*problem* (problem pddl-plan))
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
               initially
               (best-first-mfp movements :verbose verbose)
               then
               (funcall handler cost))
          (while handler)
          (for cost = (funcall evaluator
                               *problem*
                               ss
                               schedule
                               movements
                               component
                               :verbose verbose))
          (when (< cost best-cost)
            (when verbose
              (format t "~&Best plan updated: ~@{~a~^, ~}"
                      plan ss cost))
            (setf best-plan plan
                  best-ss ss
                  best-cost cost)))))

@export
(defun exploit-loop-problems (pddl-plan component evaluator
                              &key verbose (timeout MOST-POSITIVE-FIXNUM))
  "Returns 3 values: loop-path, ss, the real-cost of the loop path returned
by the evaluator."
  (when verbose (format t "~&Running MFP ..."))
  (let ((s *standard-output*)
        (e *error-output*)
        best-plan best-ss
        (best-cost MOST-POSITIVE-FIXNUM))
    (more-labels () (%exploit-main)
      (let ((wait (make-thread (lambda () (sleep timeout))))
            (main (make-thread #'%exploit-main)))
        (tagbody
           start
           (unless (thread-alive-p wait)
             (format *error-output* "~&Timeout! Search stopped.")
             (destroy-thread main)
             (go end))
           (unless (thread-alive-p main)
             (format t "~&Search finished!")
             (destroy-thread wait)
             (go end))
           end)
        (values best-plan best-ss best-cost)))))
