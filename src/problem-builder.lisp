(in-package :pddl.loop-detection)
(use-syntax :annot)

@export
(defun gen-base (n)
 (gensym (format nil "BASE~a-" n)))

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
     (let* ((objects (categorize objs :key (rcurry #'pddl-typep type)))
	    (objects/bases (gethash nil objects))
	    (bases         (gethash t   objects))
	    (init/bases (remove-if (lambda (state)
				     (some (curry #'related-to state) bases))
				   init))
	    (mutices (mutex-predicates (domain unit-problem)))
	    (ss (car loop-plan)))  ; steady-state start
       (let ((*problem*
	      (pddl-problem
	       :name (apply #'concatenate-symbols
			    unit-name 'STEADY-STATE ss)
	       :objects objects/bases
	       :init init/bases
	       :metric metric)))
	 (iter
	   (for position in ss)
	   (for base = (pddl-object :name (gen-base position) :type type))
	   (for owners = (nth position movements-shrinked))
	   
	   
	   
	   
	   (iter
	     (for owner in owners)
	     (match (find-if (compose (curry #'eqname owner) #'first) mutices)
	       ((list (pddl-predicate :name oname)
		      (pddl-predicate :name mname)
		      indices kind _)
		(let* ((oparam 
			(substitute-if base
				       (rcurry #'pddl-typep type)
				       (parameters owner)))
		       (mparam (mapcar (rcurry #'nth oparam) indices)))
		  (push (pddl-atomic-state :name oname :parameters oparam)
			(init *problem*))
		  (let ((mutex (pddl-atomic-state :name mname :parameters mparam)))
		    (case kind
		      (:positive (push mutex (init *problem*)))
		    (:negative
		     (setf (init *problem*)
			   (remove-if 
			    (lambda (state)
			      (if (not (typep state 'pddl-atomic-state))
				  t
				  (eqstate mutex state)))
			    (init *problem*)))))))))))
	 (break+ (name *problem*) (init *problem*))
	 *problem*)))))