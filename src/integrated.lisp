
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
      (let ((schedule (reschedule unit-plan
                                  :minimum-slack
                                  :verbose nil)))
        (multiple-value-bind (movements movements-indices)
            (extract-movements base-object schedule *domain*)
          (message)
          (return-from exploit-loop-problems
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
             base-type)))))))


;; base-object: symbol, keyword, string
;; returns a lazy list of (list problempath 
@export
(defun exploit-loop-problems-lazy (unit-plan base-object &key verbose)
  (terpri *standard-output*)
  (let* ((*problem* (problem unit-plan))
         (*domain* (domain unit-plan))
         (tmpdir (mktemp))
         (base-type (type (object *problem* base-object))))
    (let ((schedule (reschedule unit-plan
                                :minimum-slack
                                :verbose nil)))
      (multiple-value-bind (movements movements-indices)
          (extract-movements base-object schedule *domain*)
        (values
         (let ((problem *problem*)) ;; this should be lexical
           (label1 rec (cont)
               (ematch (funcall cont)
                 ((cons loop-plans rest-lazy)
                  (lcons (list (write-problem
                                (build-steady-state-problem
                                 problem
                                 (car loop-plans) ;; uses the first path only
                                 schedule
                                 movements
                                 movements-indices
                                 base-type)
                                tmpdir)
                               (caar loop-plans))
                         (rec rest-lazy)))
                 (nil nil))
             (curry #'rec
              (exploit-loopable-steady-state-lazy
               movements
               (exploit-steady-state-lazy movements)
               :verbose verbose))))
         base-type)))))
