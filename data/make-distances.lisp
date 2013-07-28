(in-package :pddl.loop-detection)
(defun make-dists (reaches)
  (let* (result	 
	 (places (map 'vector #'third reaches))
	 (dists (map 'vector (lambda (x)
			       (declare (ignore x))
			       (1+ (random 3)))
		     (cdr reaches))))
    ;; this is necessary because translate.py throws an error
    ;; "Could not find instantiation for PNE!"
    ;; 
    ;; according to http://www.ida.liu.se/~TDDD48/labs/2012/planners.en.shtml ,
    ;; 
    ;; A PNE is a PrimitiveNumericExpression. This most likely means
    ;; that you are writing a cost-based domain with costs defined by a
    ;; function, and that the function does not specify the costs for
    ;; all combinations of arguments. Maybe you missed the case where a
    ;; UAV flies from a location to the same location?
    (iter (for p in-vector places)
	  (push `(= (move-cost ,p ,p) 1000) result))
    
    (iter (with len = (length places))
	  (for i from 1 below len)
	  (iter (for j from i below len)
		(for p1 = (elt places (- j i)))
		(for p2 = (elt places j))
		(for dist = (1+ (iter (for dist in-vector dists
					   from (- j i) below j)
				      (summing dist))))
		(push `(= (move-cost ,p1 ,p2) ,dist) result)
		(push `(= (move-cost ,p2 ,p1) ,dist) result)))
    
    (sort result #'< :key #'third)))

(make-dists '((reachable arm table-in)   ; !!! do not remove this
	      (reachable arm table-out)  ; !!! do not remove this
	      (reachable arm tray-a)
	      (reachable arm tray-b)
	      (reachable arm tray-c)
	      (reachable arm table2)
	      (reachable arm machine-a)
	      (reachable arm machine-b)))