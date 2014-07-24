(in-package :pddl.loop-detection-test)
(in-suite :pddl.loop-detection)

(defvar *movements*)

(test (movements :depends-on basis)
  (finishes
    (setf *movements*
        (extract-movements :p2 (sort-schedule *schedule*) makep)))
  (match *movements*
    ((list (list 3 (pddl-atomic-state
                    :name 'using
                    :parameters
                    (list (pddl-object :name 'm1)
                          (pddl-object :name 'p2))))
           (list 6)
           (list 9 (pddl-atomic-state
                    :name 'using
                    :parameters
                    (list (pddl-object :name 'm2)
                          (pddl-object :name 'p2))))
           (list 11))
     (pass "the schedule works as expected"))))

(test (steady-state :depends-on movements)
  (is (equalp
       '(0 (1 (2 (3)) (3)) (2 (3)) (3))
       (steady-state *movements* nil))))


(test (mfp)
  (is (equalp '(((0) (1) (2) (3) (4) (5)))
              (mutex-focused-planning *movements* '(0)))))


