(define (problem cell-assembly-p1)
  (:domain cell-assembly)
  (:objects arm - arm
	    b1 - base
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
   ;; job attributes
   (job-available-at attatch-a table-in)
   ;; job attributes
   (uses attatch-a part-a)
   ;; linear job ordering
   (depends nothing-done attatch-a)
   
   ;;;;;;;;;;;;;;;; INITIAL STATES ;;;;;;;;;;;;;;;;
   ;; 
   ;;;; Bases ;;;;;;;;;
   ;; 
   ;; All bases are at CARRY-IN
   (at b1 carry-in)
   ;; Base and jobs. All bases must have finished NOTHING-DONE
   (finished nothing-done b1)

   ;;;; Components ;;;;;;;;
   ;; 
   ;; Explain where each type of components is placed.
   (at part-a tray-a)

   ;;;; Arms ;;;;;;;;;;;;;;;;
   ;; 
   ;; Initially it can be anywhere, but I suggest you to keep
   ;; them collision-free.
   (at arm table-out)
   ;; 
   ;; Arm presence. The number of these clause would be exactly the
   ;; same as that of arms.
   (arm-present table-out)
   ;; 
   ;; All arms should be free.
   (free arm))
  (:goal (and
	  ;; In the goal state, all bases should be at CARRY-OUT
	  (at base carry-out)
	  
	  ;; Also, all base should already passed the last job.
	  (finished nothing-done base)
	  )))