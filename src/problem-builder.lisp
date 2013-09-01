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
  (match unit-problem
    ((pddl-problem :name unit-name
		   :domain *domain*
		   :objects objs
		   :init init
		   :metric metric)
     (let* ((base-type-p (rcurry #'pddl-typep type))
	    (objects (categorize objs :key (rcurry #'pddl-typep type)))
	    (objects/bases (gethash nil objects))
	    (bases         (gethash t   objects))
	    (init (categorize 
		   init :key 
		   (lambda (state)
		     (if (some (rcurry #'related-to state) bases) t nil))))
	    (init/bases (gethash nil init))
	    (init+bases (gethash t   init))
	    (mutices (mutex-predicates (domain unit-problem)))
	    (ss (car loop-plan)))  ; steady-state start
       (let* ((*problem*
	       (pddl-problem
		:name (apply #'concatenate-symbols
			     unit-name 'STEADY-STATE ss)
		:objects objects/bases ; warning!! their PROBLEM slot still refers
		:init init/bases       ; warning!! to the old problem!
		:goal '(and)
		:metric metric)))
	 (let ((new-base (pddl-object :name (gen-base '-new) :type type)))
	   (%step0 new-base)
	   (%step1 init+bases new-base base-type-p)

	   ;; Add new objects, their corresponding initial states and
	   ;; the goal conditions.
	   (iter
	     (for position in ss)
	     (for base = (pddl-object :name (gen-base position) :type type))
	     (for pbase previous base initially new-base)
	     (for prototype-atomic-states = 
		  (remove-if-not
		   (lambda (atomic-state)
		     (and (typep atomic-state 'pddl-atomic-state)
			  (some (rcurry #'related-to atomic-state) bases)))
		   (timed-state-state
		    (timed-action-end
		     (nth (1+ (nth position movements-indices-shrinked))
			  schedule)))))
	     (%step0 base)
	     (%step1 prototype-atomic-states base base-type-p)
	     (%step2 base base-type-p movements-shrinked mutices position)
	     (%step3 prototype-atomic-states pbase base-type-p)

	     (finally
	      (%step3 (positive-predicates
		       (goal unit-problem)) base base-type-p))))
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
  "Add every mutices which are accompanied with their owners to INIT
and also removes every release-predicates from it if necessary.
The owner is already added in the previous step. "
  (iter
    (for owner in (nth position movements-shrinked))
    (iter
      (for mutex in
	   (remove-if-not
	    (compose (rcurry #'%matches-to-owner-p owner) #'first)
	    mutices))
      (match mutex
	((list _ (pddl-predicate :name mname)indices kind _)
	 (let* ((oparam (substitute-if base base-type-p
				       (parameters owner)))
		(mparam (mapcar (rcurry #'nth oparam) indices))
		(new-mutex (pddl-atomic-state :name mname
					      :parameters mparam)))
	   (case kind
	     (:positive (push new-mutex (init *problem*)))
	     (:negative
	      (setf (init *problem*)
		    (remove-if 
		     (lambda (state)
		       (and (typep state 'pddl-atomic-state)
			    (eqstate new-mutex state)))
		     (init *problem*)))))))))))

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