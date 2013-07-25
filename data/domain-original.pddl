
(in-package :pddl.loop-detection-test)

(define (domain cell-assembly-original)
  (:require :typing)
  (:types arm position base part-X)
  (:predicates (AT ?ARM ?FROM)
	       (AT-JOB-A ?POS)
	       (AT-ASSEMBLE-PARTX ?POS)
	       (OCCUPIED ?TO)
	       (NOT-OCCUPIED ?TO)
	       
	       (FREE ?ARM)
	       (REACHABLE ?ARM ?TO)
	       (HOLD ?ARM ?BASE)
	       
	       (BASELOCATION ?POS)
	       (NOTBASEPLACED ?POS)
	       
	       (CONNECT ?FROM ?TO)
	       (FINISHED-JOB-A ?BASE)
	       (UNFINISH-JOB-A ?BASE)
	       (UNFINISH-JOB-B ?BASE)
	       (USED ?PART-X)
	       (UNUSED ?PART-X)
	       (ASSEMBLE-PART-B ?BASE)
	       (ASSEMBLE-PART-X ?BASE))

  (:action move-arm
	   ;;   Moves the arm (?arm) from the source
	   ;; (?from) to the destination (?to). The occupied and not-
	   ;; occupied propositions enforce a mutual exclusion con-
	   ;; straint so that only one arm can occupy any given location
	   ;; at a time.
	   :parameters (?arm - arm ?from - position ?to - position)
	   :precondition (and (at ?arm ?from)
			      (not-occupied ?to)
			      (reachable ?arm ?to))
	   :effect (and (at ?arm ?to)
			(not-occupied ?from)
			(occupied ?to)
			(not (at ?arm ?from))
			(not (occupied ?from))
			(not (not-occupied ?to))))
  
  (:action eject-base
	   ;; Eject Base: Uses an arm (?arm) to grasp and pick up a
	   ;; base (?base) from a machine or a table (?pos). Resets
	   ;; the mutual exclusion constraint enforcing the rule that
	   ;; at most 1 base can be at a location (notbaseplaced ?pos)
	   :parameters (?base - base ?arm - arm ?pos - position)
	   :precondition (and (at ?base ?pos)
			      (at ?arm ?pos)
			      (free ?arm))
	   :effect (and (hold ?arm ?base)
			(notbaseplaced ?pos)
			(not (free ?arm))
			(not (at ?base ?pos))))
  
  (:action set-base
	   ;; Set Base Base: Commands an arm (?arm) that is hold- ing
	   ;; a particular base (?base) to set the base on a machine
	   ;; or table (?pos). Each machine/table has a mutual
	   ;; exclusion constraint ensuring at most 1 base is placed
	   ;; on it (notbase- placed).
	   :parameters (?base - base ?arm - arm ?pos - position)
	   :precondition (and (hold ?arm ?base)
			      (at ?arm ?pos)
			      (notbaseplaced ?pos)
			      (baselocation ?pos))
	   :effect (and (at ?base ?pos)
			(free ?arm)
			(not (hold ?arm ?base))
			(not (notbaseplaced ?pos))))
  
  (:action slide-base
	   ;; Slide Base: Uses a slide (carry-in/carry-out) device to
	   ;; move a base.
	   :parameters (?base - base ?from - position ?to - position)
	   :precondition (and (at ?base ?from)
			      (connect ?from ?to)
			      (notbaseplaced ?to)
			      (baselocation ?to))
	   :effect (and (at ?base ?to)
			(not (at ?base ?from))
			(not (notbaseplaced ?to))
			(notbaseplaced ?from)))
  
  (:action pickup-part-X
	   ;; Pick Parts by Arm: Use an arm (?arm) to pick up a part
	   ;; (?part). The part will later be used by a BaseAssem-
	   ;; blePickedPartsXByArm action (see below).
	   :parameters (?part-X - part-X ?arm - arm ?pos - position)
	   :precondition (and (free ?arm)
			      (at ?arm ?pos)
			      (at ?part-X ?pos))
	   :effect (and (hold ?arm ?part-X)
			(not (at ?part-X ?pos))
			(not (free ?arm))))
  
  (:action Base-Assemble-JobA-by-Machine
	   ;; Base Assemble by Machine: Use a machine (?pos) to
	   ;; perform an assembly operation (e.g., tighten the screw) on
	   ;; a base (?base).

	   ;; The ordering constraints which are determined when the
	   ;; object is designed are encoded as a set of ordering proposi-
	   ;; tions, finished_Step_X and unfihished_Step_X, which rep-
	   ;; resent whether an assembly step X has been performed
	   ;; already.
	   :parameters (?base - base ?pos - position)
	   :precondition (and (Assemble-Part-B ?base)
			      (at-Job-A ?pos)
			      (at ?base ?pos))
	   :effect (and (finished-Job-A ?base)
			(not (unfinish-Job-A ?base))))
  
  (:action Base-Assemble-Picked-PartX-by-Arm
	   ;; Base Assemble Picked Parts by Arm: Uses an arm
	   ;; (?arm) to attach a part (?part) to a base.
	   :parameters
	   (?part-X - part-X ?base - base ?arm - arm ?pos - position)
	   :precondition (and (finished-Job-A ?base)
			      (unused ?part-X)
			      (at ?base ?pos) (at ?arm ?pos)
			      (at-Assemble-PartX ?pos) (hold ?arm ?part-X))
	   :effect (and (Assemble-Part-X ?base)
			(used ?part-X)
			(free ?arm)
			(not (unused ?part-X))
			(not (hold ?arm ?part-X))
			(not (unfinish-job-b ?base)))))

