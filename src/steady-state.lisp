(in-package :pddl.loop-detection)
(use-syntax :annot)

@export
(defun make-eol (steady-state length)
  "make the end-of-loop state"
  `(,@(cdr steady-state) ,length))

@export
(defun same-end-loop-p (max a b)
  (equalp (make-eol a max)
	  (make-eol b max)))

(defun %shrink-bucket (bucket max)
  (sort
   (remove-duplicates
    bucket
    :test (curry #'same-end-loop-p max))
   #'<
   :key (lambda (lst)
	  (reduce (lambda (prev now) (+ now (* max prev))) (reverse lst)))))

@export
(defun shrink-steady-states (movements-shrinked steady-states)
  (let ((max (length movements-shrinked)))  
    (let ((v (make-array max :initial-element nil)))
      (iter (for ss in steady-states)
	    (push ss (aref v (1- (length ss)))))
      (iter (for bucket in-vector v)
	    (appending
	     (%shrink-bucket bucket max))))))

@export
(defun exploit-steady-states (movements-shrinked)
  (let ((max (length movements-shrinked)))
    (mapcon
     (lambda (rest)
       (%exploit-rec rest
		     nil
		     nil
		     (- max (length rest))))
     movements-shrinked)))

(defun %exploit-rec (movements-shrinked used-mutices base-positions i)
  ;(break+ used-mutices (car movements-shrinked))
  ;; (print (reverse
  ;; 	     (cons i base-positions)))
  (match movements-shrinked
    ((list this)
     (%exploit-leaf this used-mutices base-positions i))
    ((list* this rest)
     (if (null (intersection this used-mutices
			     :test #'eqstate))
	 (list* (reverse (cons i base-positions))
		(mapcon
		 (lambda (rest2)
		   (%exploit-rec
		    rest2
		    (append this used-mutices)
		    (cons i base-positions)
		    (+ 1 i (- (length rest)
			      (length rest2)))))
		 rest))
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
      
	
