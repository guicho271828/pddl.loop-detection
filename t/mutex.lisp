(in-package :pddl.loop-detection-test)
(in-suite :pddl.loop-detection)

;; product: p1, p2    type: product

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




