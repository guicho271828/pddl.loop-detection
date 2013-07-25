
(:action add-task
	 :parameters (?required-job ?next-job - job ?base - base)
	 :precondition (forall (?required-job)
			       (and (finished ?required-job ?base)
				    (depends-on ?next-job ?required-job)))
	 