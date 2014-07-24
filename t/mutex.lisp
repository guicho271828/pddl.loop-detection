(in-package :pddl.loop-detection-test)
(in-suite :pddl.loop-detection)

;; product: p1, p2    type: product

(define (domain make)
  (:requirements :strips :action-cost)
  (:types machine step product)
  (:predicates (available ?m - machine)
               (use ?m - machine ?s - step)
               (notusing ?p - product)
               (using ?m - machine ?p - product)
               (made ?p - product ?step - step))
  (:functions (total-cost)
              (span ?p - product ?s - step))
  (:action start
           :parameters (?p - product ?s - step ?m - machine)
           :precondition (and (available ?m) (use ?m ?s) (notusing ?p))
           :effect (and (using ?m ?p)
                        (not (notusing ?p))
                        (not (available ?m))
                        (increase (total-cost) 1)))
  (:action make
           :parameters (?p - product ?s1 ?s2 - step ?m - machine)
           :precondition (and (using ?m ?p) (made ?p ?s1))
           :effect (and (not (made ?p ?s1)) (made ?p ?s2)
                        (increase (total-cost) (span ?p ?s2))))
  (:action end
           :parameters (?p - product ?s - step ?m - machine)
           :precondition (and (using ?m ?p) (made ?p ?s))
           :effect (and (not (using ?m ?p))
                        (notusing ?p)
                        (available ?m)
                        (increase (total-cost) 1))))

(test subsumption-start
  (let ((start (action make :start)))
    (with-add-effects (using) start
      (with-delete-effects (notusing available) start
        (is-true (subset-effect-p notusing using))
        (is (equalp '(1) (index-mapping notusing using)))
        (is (specializes using notusing (index-mapping notusing using)))
        (multiple-value-bind (ad da aa dd) (subsuming-effects-in start)
          (is-true
           (some (lambda-match
                   ((owner-lock (eq using) (eq notusing) (list 1))
                    t))
                 ad))
          (is-true
           (some (lambda-match
                   ((owner-lock (eq using) (eq available) (list 0))
                    t))
                 ad))
          (is (null da))
          (is (null aa))
          (is (null dd)))))))

(test subsumption-make
  (let ((make (action make :make)))
    (multiple-value-bind (ad da aa dd) (subsuming-effects-in make)
      (is (null ad))
      (is (null da))
      (is (null aa))
      (is (null dd)))))

(test subsumption-end
  (let ((end (action make :end))) 
    (with-delete-effects (using) end
      (with-add-effects (notusing available) end
        (multiple-value-bind (ad da aa dd) (subsuming-effects-in end)
          (is (null ad))
          (mapcar #'print (list da using notusing available))
          (is-true
           (some (lambda-match
                   ((owner-lock (eq using) (eq notusing) (list 1)) t))
                 da))
          (is-true
           (some (lambda-match
                   ((owner-lock (eq using) (eq available) (list 0)) t))
                 da))
          (is (null aa))
          (is (null dd))

          (dolist (owl da)
            (dolist (a (actions make))
              (is (owner-releaser-valid-p owl a)))))))))

(test mutex-predicates
  (let ((owls (mutex-predicates make)))
    (is-true
     (some (lambda-match
             ((owner-lock (pddl-predicate :name 'using)
                          (pddl-predicate :name 'notusing)
                          (list 1)) t))
           owls))
    (is-true
     (some (lambda-match
             ((owner-lock (pddl-predicate :name 'using)
                          (pddl-predicate :name 'available)
                          (list 0)) t))
           owls))))




