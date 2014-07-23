(in-package :pddl.loop-detection-test)
(in-suite :pddl.loop-detection)

(test extract-movements
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


