(define (problem cell-assembly-p1)
  (:domain cell-assembly)
  (:objects arm - arm
	    base - base
	    - table
	    - machine
	    - job
	    - machine-job
	    )
  (:init
   ;;;;;;;;;;;;;;;; ATTRIBUTES ;;;;;;;;;;;;;;;;
   ;; 
   ;; arm attributes
   (reachable arm table-in)   ; !!! do not remove this
   (reachable arm table-out)  ; !!! do not remove this
   ;; conveyor attributes
   (connected carry-in table-in)   ; !!! do not remove this
   (connected table-out carry-out) ; !!! do not remove this
   ;; machine attributes
   
   ;; linear job ordering

   ;;;;;;;;;;;;;;;; INITIAL STATES ;;;;;;;;;;;;;;;;
   ;; 
   ;; All bases are at CARRY-IN
   (at base carry-in)

   ;; Base and jobs. All bases must have finished NOTHING-DONE
   (finished nothing-done base)

   ;; Arms. Initially it can be anywhere, but I suggest you to keep
   ;; them collision-free.
   (at arm carry-out)

   ;; Arm presence. The number of these clause would be exactly the
   ;; same as that of arms.
   (arm-present carry-out)

   ;; All arms should be free.
   (free arm))
  (:goal (and
	  ;; In the goal state, all bases should be at CARRY-OUT
	  (at base carry-out)
	  
	  ;; Also, all base should already passed the last job.
	  (finished nothing-done base)
	  )))