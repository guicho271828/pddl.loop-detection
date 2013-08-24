(in-package :pddl.loop-detection)
(use-syntax :annot)

@export
(defun make-eol (steady-state length)
  "make the end-of-loop state"
  `(,@(cdr steady-state) ,length))

@export
(defun exploit-steady-state (movements)
  (let ((max (length movements)))
    (let ((unshrinked (mapcon
		       (lambda (rest)
			 (%exploit-rec rest
				       nil
				       nil
				       (- max (length rest))))
		       movements))
	  (v (make-array max :initial-element nil)))
      (iter (for ss in unshrinked)
	    (push ss (aref v (length ss))))
      (iter (for bucket in-vector v)
	    (appending
	     (remove-duplicates
	      bucket
	      :test (lambda (a b)
		      (or (equalp a b)
			  (equalp (make-eol a max)
				  (make-eol b max))))))))))

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
      
	
