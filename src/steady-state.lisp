(in-package :pddl.loop-detection)
(use-syntax :annot)

@export
(defun exploit-steady-state (movements)
  (remove-duplicates
   (mapcon
    (lambda (rest)
      (%exploit-rec rest
		    nil
		    nil
		    (- (length movements)
		       (length rest))))
    movements)
   :test #'equalp))

(defun %exploit-rec (movements used-mutices base-positions i)
  ;(break+ used-mutices (car movements))
  (match movements
    ((list this)
     (%exploit-leaf this used-mutices base-positions i))
    ((list* this rest)
     (if (null (intersection this used-mutices
			     :test #'eqstate))
	 (mapcon
	  (lambda (rest2)
	    (%exploit-rec
	     rest2
	     (append this used-mutices)
	     (cons i base-positions)
	     (+ 1 i (- (length rest)
		     (length rest2)))))
	  rest)
	 (%exploit-rec rest
		       used-mutices
		       base-positions
		       (1+ i))))))

(defun %exploit-leaf (this used-mutices base-positions i)
  (if (null (intersection this used-mutices
			  :test #'eqstate))
      (list (reverse
	     (cons i base-positions)))
      nil))
      
	
