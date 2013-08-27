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
    ((pddl-problem
      :name unit-name
      :domain *domain*
      :objects objs
      :init init
      :metric metric)
     (let* ((base-type-p (rcurry #'pddl-typep type))
	    (objects (categorize objs :key (rcurry #'pddl-typep type)))
	    (objects/bases (gethash nil objects))
	    (bases         (gethash t   objects))
	    (init/bases (remove-if (lambda (state)
				     (some (curry #'related-to state) bases))
				   init))
	    (mutices (mutex-predicates (domain unit-problem)))
	    (ss (car loop-plan)))  ; steady-state start
       (let* ((*problem*
	       (pddl-problem
		:name (apply #'concatenate-symbols
			     unit-name 'STEADY-STATE ss)
		:objects objects/bases ; warning!! their PROBLEM slot still refers to
		:init init/bases       ; warning!! the old problem!
		:metric metric)))
	 (iter
	   (for position in ss)
	   (for base = (pddl-object :name (gen-base position) :type type))
	   (for prototype-atomic-states = 
		(remove-if-not
		 (lambda (atomic-state)
		   (and (typep atomic-state 'pddl-atomic-state)
			(some (rcurry #'related-to atomic-state) bases)))
		 (timed-state-state
		  (timed-action-end
		   (nth (1+ (nth position movements-indices-shrinked))
			schedule)))))
	   
	   ;; Insert every states which is describing the base in the
	   ;; particular step in the unit plan, replacing the base
	   ;; with a new object in the steady-state
	   (appendf (init *problem*)
		    (break+
		     base
		     (nth (1+ (nth position movements-indices-shrinked))
			schedule)
		     prototype-atomic-states
		     (iter
		       (for proto in prototype-atomic-states)
		       (match proto
			 ((pddl-atomic-state name parameters)
			  (collect
			       (pddl-atomic-state
				:name name
				:parameters (substitute-if base
							   base-type-p
							   parameters))))))))
		
	   (iter
	     (for owner in (nth position movements-shrinked))
	     (iter
	       (for mutex in (remove-if-not
			      (compose (rcurry #'%matches-to-owner-p owner)
				       #'first)
			      mutices))
	       (match mutex
		 ((list (pddl-predicate :name oname)
			(pddl-predicate :name mname)
			indices kind _)
		  (let* ((oparam (substitute-if base base-type-p (parameters owner)))
			 (mparam (mapcar (rcurry #'nth oparam) indices))
			 (new-owner (pddl-atomic-state :name oname :parameters oparam))
			 (new-mutex (pddl-atomic-state :name mname :parameters mparam)))
		    (break+ (nth (1+ (nth position movements-indices-shrinked))
				 schedule)
			    prototype-atomic-states
			    owner
			    mutex
			    new-owner
			    new-mutex)
		    (push new-owner (init *problem*))
		    (case kind
		      (:positive (push new-mutex (init *problem*)))
		      (:negative
		       (setf (init *problem*)
			     (remove-if 
			      (lambda (state)
				(and (typep state 'pddl-atomic-state)
				     (eqstate new-mutex state)))
			      (init *problem*)))))))))))
	 (let ((*print-circle* nil))
	   (break+ (name *problem*)
		   (states-only (init *problem*))
		   (set-difference
		    (states-only (init *problem*))
		    (remove-duplicates 
		     (states-only (init *problem*))
		     :test #'eqstate))))
	 *problem*)))))