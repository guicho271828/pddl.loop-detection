(in-package :pddl.loop-detection-test)
(in-suite :pddl.loop-detection)

(defvar +makeplan+
  '((start p1 s1 m1) (make p1 s0 s1 m1) (end p1 s1 m1)
    (start p1 s2 m2) (make p1 s1 s2 m2) (end p1 s2 m2)
    (start p2 s1 m1) (make p2 s0 s1 m1) (end p2 s1 m1)
    (start p2 s2 m2) (make p2 s1 s2 m2) (end p2 s2 m2)))

(define (problem makep)
  (:domain make)
  (:objects m1 m2 - machine p1 p2 - product s0 s1 s2 - step)
  (:init (made p1 s0)
         (made p2 s0)
         (notusing p1)
         (notusing p2)
         (available m1)
         (available m2)
         (use m1 s1)
         (use m2 s2)
         (= (total-cost) 0)
         (= (span p1 s0) 0)
         (= (span p1 s1) 3)
         (= (span p1 s2) 5)
         (= (span p2 s0) 0)
         (= (span p2 s1) 2)
         (= (span p2 s2) 3))
  (:goal (and (made p1 s2) (made p2 s2)))
  (:metric minimize (total-cost)))

(defvar *schedule*)

(test basis
  (let ((*domain* make) (*problem* makep)) ; see t/data.lisp
    (let* ((actions (parse-plan +makeplan+)) ; see t/data.lisp
           (plan (pddl-plan :actions actions))
           (env (pddl-environment :plan plan))
           (last-env (simulate-plan env)))
      (is (= 21 (cost last-env)))
      (is-true (goal-p makep (states last-env)))
      (setf *schedule*
            (sort-schedule
             (reschedule plan :minimum-slack :verbose t)))
      (print-timed-action-graphically *schedule*)
      (is (= 17 (timed-state-time (timed-action-end (lastcar *schedule*))))))))
