(define (problem cell-assembly-model2b2)
  (:domain cell-assembly)
  (:objects arm - arm
	    b1 b2  - base
	    part-a
	    part-b
	    part-c - component
	    
	    tray-a
	    tray-b
	    tray-c - tray
	    
	    table2 - table
	    
	    machine-a
	    machine-b - machine
	    attatch-a
	    attatch-b
	    attatch-c - job
	    screw-a
	    screw-c - machine-job
	    )
  (:init
   ;;;;;;;;;;;;;;;; ATTRIBUTES ;;;;;;;;;;;;;;;;
   ;; 
   ;; arm attributes
   (reachable arm table-in)   ; !!! do not remove this
   (reachable arm table-out)  ; !!! do not remove this
   (reachable arm tray-a)
   (reachable arm tray-b)
   (reachable arm tray-c)
   (reachable arm table2)
   (reachable arm machine-a)
   (reachable arm machine-b)
   ;; conveyor attributes
   (connected carry-in table-in)   ; !!! do not remove this
   (connected table-out carry-out) ; !!! do not remove this
   ;; job attributes
   (job-available-at attatch-a table-in)
   (job-available-at screw-a machine-a)
   (job-available-at attatch-b table2)
   (job-available-at attatch-c table2)
   (job-available-at screw-c machine-b)
   ;; job attributes
   (uses attatch-a part-a)
   (uses attatch-b part-b)
   (uses attatch-c part-c)
   ;; linear job ordering
   (depends nothing-done attatch-a)
   (depends attatch-a screw-a)
   (depends screw-a attatch-b)
   (depends attatch-b attatch-c)
   (depends attatch-c screw-c)
   
   ;;;;;;;;;;;;;;;; INITIAL STATES ;;;;;;;;;;;;;;;;
   ;; 
   ;;;; Bases ;;;;;;;;;
   ;; 
   ;; All bases are at CARRY-IN
   (at b1 carry-in)
   (at b2 carry-in)
   ;; Base and jobs. All bases must have finished NOTHING-DONE
   (finished nothing-done b1)
   (finished nothing-done b2)

   ;;;; Components ;;;;;;;;
   ;; 
   ;; Explain where each type of components is placed.
   (at part-a tray-a)
   (at part-b tray-b)
   (at part-c tray-c)

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
	  (at b1 carry-out)
	  (at b2 carry-out)
	  
	  ;; Also, all base should already passed the last job.
	  (finished screw-c b1)
	  (finished screw-c b2)
	  )))