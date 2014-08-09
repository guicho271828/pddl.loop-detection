
(in-package :pddl.loop-planner-test)
(in-suite :pddl.loop-planner)

;; tests with-timeout and run-program

(test exploit-loop-problems-with-evaluation
  (finishes
    (let ((*domain* make) (*problem* makep))
      (exploit-loop-problems
       (pddl-plan :actions (parse-plan +makeplan+))
       (object *problem* :p1)
       #'evaluate-loop-problem
       :verbose t))))

(test exploit-loop-problems-with-timeout
  (finishes
    (let ((*domain* make) (*problem* makep))
      (time
       (exploit-loop-problems
        (pddl-plan :actions (parse-plan +makeplan+))
        (object *problem* :p1)
        #'evaluate-loop-problem
        :verbose t
        :timeout 2)))))

(test elp-with-e-with-component
  (finishes
    (let ((*domain* assemble) (*problem* assemblep))
      (describe
       (exploit-loop-problems
        (pddl-plan :actions (parse-plan +assembleplan+))
        (list (object *problem* :p1)
              (object *problem* :pa11)
              (object *problem* :pa12))
        #'evaluate-loop-problem)))))
