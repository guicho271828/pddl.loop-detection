(define (problem cell-assembly-with-cost-p1)
  (:domain cell-assembly-cost)
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
   ;; cost initialization
   (= (total-cost) 0)   ; !!! do not remove this
   (= (loading-cost) 1) ; !!! do not remove this

   ;; arm attributes
   (reachable arm table-in)   ; !!! do not remove this
   (reachable arm table-out)  ; !!! do not remove this
   
   ;; position attributes
   (= (move-cost table-in table-out) 3)
   (= (move-cost table-out table-in) 3) ;; this is awful
   
   ;; conveyor attributes
   (connected carry-in table-in)   ; !!! do not remove this
   (connected table-out carry-out) ; !!! do not remove this
   ;; job attributes (place)
   (job-available-at attatch-a table-in)
   ;; job attributes (required components)
   (uses attatch-a part-a)
   ;; job attributes (cost)
   (= (job-cost attatch-a) 5)
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
  (:metric (minimize (total-cost)))
  (:goal (and
	  ;; In the goal state, all bases should be at CARRY-OUT
	  (at base carry-out)
	  
	  ;; Also, all base should already passed the last job.
	  (finished nothing-done base)
	  )))