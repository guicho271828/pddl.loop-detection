(in-package :pddl.loop-detection)
(use-syntax :annot)

@export
(defun gen-base (n)
 (gensym (format nil "BASE~a-" n)))

(defun states-only (states)
  (remove-if-not (rcurry #'typep 'pddl-atomic-state) states))

@export
(defun build-steady-state-problem (unit-problem
                                   loop-plan
                                   schedule
                                   movements-shrinked
                                   movements-indices-shrinked
                                   type)
  (ematch unit-problem
    ((pddl-problem :name unit-name
                   :domain *domain*
                   :objects objs
                   :init init
                   :metric metric)
     (let* ((base-type-p (rcurry #'pddl-typep type))
            (objects (categorize objs :key base-type-p))
            (objects/bases (gethash nil objects))
            (bases         (gethash t   objects))
            (init (categorize 
                   init :key 
                   (lambda (state)
                     (if (some (rcurry #'related-to state) bases) t nil))))
            (init/bases (gethash nil init))
            (mutices (mutex-predicates *domain*))
            (ss (car loop-plan)))  ; steady-state start
       (let* ((*problem*
               (pddl-problem
                :name (apply #'concatenate-symbols
                             unit-name 'STEADY-STATE ss)
                :objects objects/bases ; warning!! their PROBLEM slot still refers
                :init init/bases       ; warning!! to the old problem!
                :goal (list 'and)
                :metric metric)))

         ;; Add new objects, their corresponding initial states and
         ;; the goal conditions.
         (iter
           ;; example: ss = (0 1 2 9)
           (for position in ss)
           (for base = (pddl-object :name (gen-base position) :type type))
           (for prototype-atomic-states = 
                (remove-if-not
                 (lambda (atomic-state)
                   (and (typep atomic-state 'pddl-atomic-state)
                        (some (rcurry #'related-to atomic-state) bases)))
                 (timed-state-state
                  (timed-action-end
                   (nth (nth position movements-indices-shrinked)
                        schedule)))))
           (%step0 base)
           (%step1 prototype-atomic-states base base-type-p)
           (%step2 base base-type-p movements-shrinked mutices position)
           
           (for pbase previous base)
           (unless pbase (next-iteration))
           (%step3 prototype-atomic-states pbase base-type-p)

           (finally
            (%step3 (positive-predicates
                     (goal unit-problem)) base base-type-p)))
         (%step4 mutices)
         
         *problem*)))))

(defun %step0 (base)
  "Add each object."
  (push base (objects/const *problem*)))

(defun %step1 (prototype-atomic-states base base-type-p)
  "Insert every states which is describing the base in the
   particular step in the unit plan, replacing the base
   with a new object in the steady-state."
  (iter
    (for proto in prototype-atomic-states)
    (match proto
      ((pddl-atomic-state name parameters)
       (push (pddl-atomic-state
              :name name
              :parameters (substitute-if base
                                         base-type-p
                                         parameters))
             (init *problem*))))))

(defun %step2 (base base-type-p movements-shrinked mutices position)
  "Adds every mutices which are accompanied with their owners to INIT."
  (iter
    (for mutex in mutices)
    (ematch mutex
      ((list owner-pred (pddl-predicate :name mname) indices :mutex _)
       (when-let ((owner (find-if (lambda (owner)
                                    (predicate-more-specific-p owner owner-pred))
                                  (nth position movements-shrinked))))
         (let* ((oparam (substitute-if base base-type-p
                                       (parameters owner)))
                (mparam (mapcar (rcurry #'nth oparam) indices))
                (new-mutex (pddl-atomic-state :name mname
                                              :parameters mparam)))
           (push new-mutex (init *problem*)))))
      ((list* _ _ _ :release _)))))

(defun %step3 (prototype-atomic-states base base-type-p)
  "Add the goal description."
  (iter
    (for proto in prototype-atomic-states)
    (match proto
      ((pddl-atomic-state name parameters)
       (push (pddl-atomic-state
              :name name
              :parameters (substitute-if base
                                         base-type-p
                                         parameters))
             (cdr (goal *problem*)))))))

(defun %step4 (mutices)
  "Ensures every mutex-predicates is added to INIT if necessary.
Also, ensures every release-predicates is removed from INIT if necessary.
The owner is already added in the previous step."
  (iter
    (for mutex in mutices)
    (match mutex
      ((list owner-pred (pddl-predicate :name mname) indices :mutex _)
       (iter (for owner in (remove-if-not
                            (rcurry #'predicate-more-specific-p owner-pred)
                            (init *problem*)))
             (pushnew
              (pddl-atomic-state
               :name mname
               :parameters (mapcar (rcurry #'nth (parameters owner)) indices))
              (init *problem*)
              :test #'eqstate)))
      ((list owner-pred release indices :release _)
       (iter (for owner in (remove-if-not
                            (rcurry #'predicate-more-specific-p owner-pred)
                            (init *problem*)))
             (setf (init *problem*)
                   (remove-if 
                    (lambda (state)
                      (and (typep state 'pddl-atomic-state)
                           (%matches-to-mutex-p release indices owner state)))
                    (init *problem*))))))))

