(in-package :pddl.loop-detection-test)
(in-suite :pddl.loop-detection)

;; product: p1, p2    type: product

(define (domain make)
  (:requirements :strips :action-cost)
  (:types machine step product)
  (:predicates (available ?m - machine)
               (use ?m - machine ?s - step)
               (notmaking ?p - product)
               (making ?m - machine ?p - product)
               (made ?p - product ?step - step))
  (:functions (total-cost)
              (span ?p - product ?s - step))
  (:action start
           :parameters (?p - product ?s - step ?m - machine)
           :precondition (and (available ?m) (use ?m ?s) (notmaking ?p))
           :effect (and (making ?m ?p)
                        (not (notmaking ?p))
                        (not (available ?m))
                        (increase (total-cost) 1)))
  (:action make
           :parameters (?p - product ?s1 ?s2 - step ?m - machine)
           :precondition (and (making ?m ?p) (made ?p ?s1))
           :effect (and (not (made ?p ?s1)) (made ?p ?s2)
                        (increase (total-cost) (span ?p ?s2))))
  (:action end
           :parameters (?p - product ?s - step ?m - machine)
           :precondition (and (making ?m ?p) (made ?p ?s))
           :effect (and (not (making ?m ?p))
                        (notmaking ?p)
                        (available ?m)
                        (increase (total-cost) 1))))

(test subsumption-start
  (let ((start (action make :start)))
    (with-add-effects (making) start
      (with-delete-effects (notmaking available) start
        (is-true (subset-effect-p notmaking making))
        (is (equalp '(1) (index-mapping notmaking making)))
        (is (specializes making notmaking (index-mapping notmaking making)))
        (multiple-value-bind (ad da aa dd) (subsuming-effects-in start)
          (is-true
           (some (lambda-match
                   ((owner-lock (eq making) (eq notmaking) (list 1))
                    t))
                 ad))
          (is-true
           (some (lambda-match
                   ((owner-lock (eq making) (eq available) (list 0))
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
    (with-delete-effects (making) end
      (with-add-effects (notmaking available) end
        (multiple-value-bind (ad da aa dd) (subsuming-effects-in end)
          (is (null ad))
          (mapcar #'print (list da making notmaking available))
          (is-true
           (some (lambda-match
                   ((owner-lock (eq making) (eq notmaking) (list 1)) t))
                 da))
          (is-true
           (some (lambda-match
                   ((owner-lock (eq making) (eq available) (list 0)) t))
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
             ((owner-lock (pddl-predicate :name 'making)
                          (pddl-predicate :name 'notmaking)
                          (list 1)) t))
           owls))
    (is-true
     (some (lambda-match
             ((owner-lock (pddl-predicate :name 'making)
                          (pddl-predicate :name 'available)
                          (list 0)) t))
           owls))))




