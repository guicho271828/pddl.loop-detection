
(in-package :pddl.loop-detection)
(cl-syntax:use-syntax :annot)

@export
(defclass evaluation-result ()
  ((ss :reader ss :initarg :ss)
   (cost :reader cost :initarg :cost)))

@export
(defun evaluation-result-< (result1 result2)
  (< (cost result1) (cost result2)))

@export
(defun evaluation-result-min (result1 result2)
  (if (evaluation-result-< result1 result2)
      result1 result2))

(declaim (ftype (function
                 (pddl-plan t (function (pddl-problem
                                         list list list list
                                         &key (:verbose boolean))
                                        evaluation-result)
                            &key
                            (:verbose boolean)
                            (:timeout fixnum))
                 evaluation-result)
                exploit-loop-problems))

(define-local-function %exploit-main ()
  (let* ((*standard-output* s)
         (*error-output* e)
         (*problem* (problem pddl-plan))
         (*domain* (domain pddl-plan))
         (component (ensure-list component))
         (schedule (sort-schedule
                    (reschedule pddl-plan
                                :minimum-slack
                                :verbose verbose)))
         (movements
          (iter (for c in component)
                (reducing
                 (extract-movements c schedule)
                 by #'merge-movements))))
    (iter (for (values plan ss handler)
               initially (best-first-mfp movements :verbose verbose)
               then (funcall handler (cost result)))
          (while handler)
          (for result = 
               (funcall evaluator *problem* ss
                        schedule movements component
                        :verbose verbose))
          (when (evaluation-result-< result best)
            (when verbose (format t "~&Best plan updated: ~a" result))
            (setf best result)))))

@export
(defun exploit-loop-problems (pddl-plan component evaluator
                              &key verbose (timeout MOST-POSITIVE-FIXNUM))
  "Returns 3 values: loop-path, ss, the real-cost of the loop path returned
by the evaluator."
  (when verbose (format t "~&Running MFP ..."))
  (let ((s *standard-output*)
        (e *error-output*)
        (best (make-instance
               'evaluation-result
               :ss nil :cost MOST-POSITIVE-FIXNUM)))
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
        best))))

@export
(defun constant-results (n)
  (lambda (p ss &rest args)
    (declare (ignore p args))
    (make-instance 'evaluation-result :ss ss :cost n)))
