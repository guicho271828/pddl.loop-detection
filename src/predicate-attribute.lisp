
(in-package :pddl.loop-detection)
(use-syntax :annot)

;;よくわからん

@export
(defun related-actions (predicate)
  (match predicate
    ((pddl-predicate name domain)
     (remove-if-not
      (named-lambda per-action (action)
	(walk-tree
	 (lambda (branch cont)
	   (match branch
	     ((op _ preds) (funcall cont preds))
	     ((pddl-predicate :name name2)
	      (when (eq name name2)
		(return-from per-action t)))))
	 (precondition action))
	nil)
      (actions domain)))))

@export
(defun mutex-predicates (predicate)
  (iter (for action in (related-actions predicate))
	(appending (%mutex-predicate predicate action))))

@export
(defun %mutex-predicate (predicate action)
  (let ((candidate1 nil)
	(candidate2 nil)
	(all-effects (append (add-list action)
			     (delete-list action))))
    (flet ((push-candidates (pred mutex-candidate)
	     (match pred
	       ((pddl-predicate :parameters params)
		(match mutex-candidate
		  ((pddl-predicate
		    :parameters
		    (guard params2 (subsetp params2 params)))
		   (push pred candidate1)
		   (push mutex-candidate candidate2)))))))
      (map-product
       #'push-candidates
       (remove-if-not (curry #'eqname predicate) (add-list action))
       (remove-if     (curry #'eqname predicate) (add-list action)))
      (map-product
       #'push-candidates
       (remove-if-not (curry #'eqname predicate) (delete-list action))
       (remove-if     (curry #'eqname predicate) (delete-list action)))

      (iter (for pred in candidate1)
	    (for mutex in candidate2)
	    (for indices = (get-mutex-argument-indices
			    pred mutex))
	    (when
		(every
		 ;; もし同じpredicateの削除効果があれば、
		 ;; 対応したmutex開放もないといけない
		 (lambda (pred2)
		   (let ((mutex-arguments
			  (mapcar (rcurry #'nth (parameters pred2))
				  indices)))
		     (some
		      (lambda (mutex?)
			(equalp (parameters mutex?) mutex-arguments))
		      (remove-if-not (curry #'eqname mutex) all-effects))))
		 (remove-if-not (curry #'eqname predicate) all-effects))
	      (collect (list pred mutex)))))))

     
(defun get-mutex-argument-indices (predicate mutex)
  (iter (for p in (parameters mutex))
	(collect
	    (position p (parameters predicate)))))
		
@export
(defun movement-style-action-p (predicate action)
  )



