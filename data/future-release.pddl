
(:action add-task
	 :parameters (?required-job ?next-job - job ?base - base)
	 :precondition (forall (?required-job)
			       (and (finished ?required-job ?base)
				    (depends-on ?next-job ?required-job))))

(:derived (arm-present ?pos - position)
	  (exists (?arm - arm)
		  (at ?arm ?pos)))
(:derived (base-present ?pos - position)
	  (exists (?base - base)
		  (at ?base ?pos)))
