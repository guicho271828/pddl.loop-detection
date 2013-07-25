(define (problem cell-assembly-p2)
  (:domain cell-assembly)
  (:objects arm - arm
	    base - base
	    - table
	    machine - machine
	    - job
	    screw - machine-job
	    )
  (:init
   ;; arm attributes
   (reachable arm table-in)   ; !!! do not remove this
   (reachable arm table-out)  ; !!! do not remove this
   (reachable arm machine)
   
   ;; conveyor attributes
   (connected carry-in table-in)   ; !!! do not remove this
   (connected table-out carry-out) ; !!! do not remove this
   
   ;; machine attributes
   (job-available-at screw machine)
   
   ;; linear job ordering
   (depends nothing-done screw)

   ;; initial states
   ;; bases
   (at base carry-in)

   ;; base and jobs
   (finished nothing-done base)

   ;; tables
   
   ;; arms
   (at arm table-out)
   (arm-present carry-out)
   (free arm))
  (:goal (and (at base carry-out)
	      (finished screw base)
	      )))