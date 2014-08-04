
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


(defparameter *default-special-bindings*
  `((*standard-output* . ,*standard-output*)
    (*error-output* . ,*error-output*)
    (*trace-output* . ,*trace-output*)
    (*standard-input* . ,*standard-input*)
    (*query-io* . ,*query-io*)
    (*debug-io* . ,*debug-io*)))


;; in order to make it work on 32bit systems
@export
(defparameter *memory-limit*
  #+X86
  :infinity ;MOST-POSITIVE-FIXNUM
  #+X86-64
  (rlimit +rlimit-address-space+))
@export
(defparameter *soft-time-limit*
  #+X86
    :infinity ;MOST-POSITIVE-FIXNUM
  #+X86-64
  (rlimit +rlimit-cpu-time+))
@export
(defparameter *hard-time-limit*
  #+X86
  :infinity ;MOST-POSITIVE-FIXNUM
  #+X86-64
  (rlimit +rlimit-cpu-time+))

(declaim (ftype (function
                 (pddl-plan t (function (pddl-problem
                                         list list list list
                                         &key (:verbose boolean))
                                        evaluation-result)
                            &key
                            (:verbose boolean)
                            (:timeout integer)
                            (:soft-timeout-per-ss (or keyword integer))
                            (:hard-timeout-per-ss (or keyword integer))
                            (:memory-limit-per-ss (or keyword integer)))
                 evaluation-result)
                exploit-loop-problems))

(define-local-function %exploit-main ()
  (let* ((*problem* (problem pddl-plan))
         (*domain* (domain pddl-plan))
         (component (ensure-list component))
         (schedule (sort-schedule
                    (reschedule pddl-plan
                                :minimum-slack
                                :verbose verbose)))
         (movements
          (sort (iter (for c in component)
                      (reducing
                       (extract-movements c schedule)
                       by #'merge-movements))
                #'< :key #'movement-index))
         (*soft-time-limit* soft-timeout-per-ss)
         (*hard-time-limit* hard-timeout-per-ss)
         (*memory-limit* memory-limit-per-ss))
    (iter (for (values plan ss handler)
               initially (best-first-mfp movements :verbose verbose)
               then (funcall handler (cost result)))
          (while handler)
          (when (keywordp plan) ; :duplicated or :fail
            (setf result worst) ; current ss should not be evaluated any
                                ; more
            (next-iteration))
          (for result = 
               (funcall evaluator *problem* ss
                        schedule movements component
                        :verbose verbose))
          (when (evaluation-result-< result best)
            (when verbose (format t "~&Best plan updated: ~a" result))
            (setf best result)))))

@export
(defun exploit-loop-problems (pddl-plan component evaluator
                              &key verbose
                                (timeout MOST-POSITIVE-FIXNUM)
                                (soft-timeout-per-ss *soft-time-limit*)
                                (hard-timeout-per-ss *hard-time-limit*)
                                (memory-limit-per-ss *memory-limit*))
  "Returns 3 values: loop-path, ss, the real-cost of the loop path returned
by the evaluator."
  (when verbose (format t "~&Running MFP ..."))
  (let* ((worst (make-instance
                 'evaluation-result
                 :ss nil :cost MOST-POSITIVE-FIXNUM))
         (best worst))
    (more-labels () (%exploit-main)
      (let ((wait (make-thread (lambda () (sleep timeout))))
            (main (make-thread #'%exploit-main)))
        (unwind-protect
             (tagbody
              start
                (unless (thread-alive-p wait)
                  (when verbose
                    (format *error-output* "~&Timeout! Search stopped."))
                  (go end))
                (unless (thread-alive-p main)
                  (when verbose
                    (format t "~&Search finished!"))
                  (go end))
                (sleep 1)
                (go start)
              end)
          (when (thread-alive-p wait) (destroy-thread wait))
          (when (thread-alive-p main) (destroy-thread main)))
        (when verbose
          (format t "~&Best result:")
          (describe best))
        best))))

@export
(defun constant-results (n)
  (lambda (p ss &rest args)
    (declare (ignore p args))
    (make-instance 'evaluation-result :ss ss :cost n)))
