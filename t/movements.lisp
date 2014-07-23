(in-package :pddl.loop-detection-test)
(in-suite :pddl.loop-detection)

(test (extract-movements :depends-on basis)
  (match (extract-movements
          :p2 (sort-schedule *schedule*) makep)
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


(test (steady-states-tree :depends-on movement)
  (is (equalp
       '(0 (1 (3 (4 5) 5) (4 5) 5) (3 (4 5) 5) (4 5) 5)
       (steady-state-tree (extract-movements :p2 *schedule* makep)))))


