(in-package :pddl.loop-detection-test)
(in-suite :pddl.loop-detection)

(define (domain assemble)
  (:requirements :strips :action-costs)
  (:types machine step product part)
  (:predicates (available ?m - machine)
               (use ?m - machine ?s - step)
               (notmaking ?p - product)
               (making ?m - machine ?p - product)
               (made ?p - product ?step - step)
               (assemble ?prod - product ?part - part ?s - step))
  (:functions (total-cost)
              (span ?p - product ?s - step))
  (:action start
           :parameters (?p - product ?s - step ?m - machine)
           :precondition (and (available ?m) (use ?m ?s) (notmaking ?p))
           :effect (and (making ?m ?p)
                        (not (notmaking ?p))
                        (not (available ?m))
                        (increase (total-cost) 1)))
  (:action assemble
           :parameters (?p - product ?s1 ?s2 - step ?m - machine
                           ?part - part)
           :precondition (and (making ?m ?p)
                              (made ?p ?s1)
                              (assemble ?p ?part ?s2))
           :effect (and (not (made ?p ?s1)) (made ?p ?s2)
                        (increase (total-cost) (span ?p ?s2))))
  (:action end
           :parameters (?p - product ?s - step ?m - machine)
           :precondition (and (making ?m ?p) (made ?p ?s))
           :effect (and (not (making ?m ?p))
                        (notmaking ?p)
                        (available ?m)
                        (increase (total-cost) 1))))

(define (problem assemblep)
  (:domain assemble)
  (:objects m1 m2 - machine
            p1 p2 - product
            s0 s1 s2 - step
            pa11 pa12 pa21 pa22 - part)
  (:init (made p1 s0)
         (made p2 s0)
         (notmaking p1)
         (notmaking p2)
         (available m1)
         (available m2)
         (use m1 s1)
         (use m2 s2)
         (assemble p1 pa11 s1)
         (assemble p1 pa12 s2)
         (assemble p2 pa21 s1)
         (assemble p2 pa22 s2)
         (= (total-cost) 0)
         (= (span p1 s0) 0)
         (= (span p1 s1) 3)
         (= (span p1 s2) 5)
         (= (span p2 s0) 0)
         (= (span p2 s1) 2)
         (= (span p2 s2) 3))
  (:goal (and (made p1 s2) (made p2 s2)))
  (:metric minimize (total-cost)))

(defvar +assembleplan+
  '((start p1 s1 m1) (assemble p1 s0 s1 m1 pa11) (end p1 s1 m1)
    (start p1 s2 m2) (assemble p1 s1 s2 m2 pa12) (end p1 s2 m2)
    (start p2 s1 m1) (assemble p2 s0 s1 m1 pa21) (end p2 s1 m1)
    (start p2 s2 m2) (assemble p2 s1 s2 m2 pa22) (end p2 s2 m2)))

(defvar *assemble-schedule*)

(test assemble
  (let ((*domain* assemble) (*problem* assemblep)) ; see t/data.lisp
    (let* ((actions (parse-plan +assembleplan+)) ; see t/data.lisp
           (plan (pddl-plan :actions actions))
           (env (pddl-environment :plan plan))
           (last-env (simulate-plan env)))
      (is (= 21 (cost last-env)))
      (is-true (goal-p assemblep (states last-env)))
      (setf *assemble-schedule*
            (sort-schedule
             (reschedule plan :minimum-slack :verbose t)))
      (print-timed-action-graphically *assemble-schedule*)
      (is (= 17 (timed-state-time (timed-action-end (lastcar *assemble-schedule*))))))))
