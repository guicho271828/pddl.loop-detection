
(in-package :pddl.loop-detection)
(use-syntax :annot)

;; 以下、mutex検知


@export
(defun subset-effect-p (effect1 effect2)
  "The basic function which checks if the given two effects are
in a owner-mutex relationship."
  (subsetp (parameters effect1)
	   (parameters effect2)))


@export
(defun mutex-predicates (domain)
  "returns all possible pairs of predicates in owner-mutex relationships."
  (shrink-mutices
   (categorize-mutices
    (validate-all-mutex-candidates domain))))

@export
(defun validate-all-mutex-candidates (domain)
  (mappend
   (lambda (a)
     (mapcar
      (curry #'%validate-mutex-among-actions
	     (remove a (actions domain)))
      (mutices-in a)))
   (actions domain)))

@export
(defun %validate-mutex-among-actions (rest-actions mutex)
  (ematch mutex
    ((list _ _ _ :positive _)
     (dolist (a2 rest-actions)
       (setf mutex (%validate-mutex mutex (delete-list a2)))
       (setf mutex (%validate-mutex mutex (add-list a2)))))
    ((list _ _ _ :negative _)
     (dolist (a2 rest-actions)
       (setf mutex (%validate-release mutex (add-list a2) (delete-list a2)))
       (setf mutex (%validate-release mutex (delete-list a2) (add-list a2))))))
  mutex)


@export
(defun mutices-in (action)
  (with-accessors ((a add-list) (d delete-list)) action
    (append
      (mapcar (rcurry #'%validate-mutex d) (%mutex-candidates a))
      (mapcar (rcurry #'%validate-mutex a) (%mutex-candidates d))
      (mapcar (rcurry #'%validate-release d a) (%release-candidates a d))
      (mapcar (rcurry #'%validate-release a d) (%release-candidates d a)))))

@export
(defun %mutex-candidates (effects)
  (when (> (length effects) 1)
    (let (pairs)
      (map-permutations
       (lambda (pair)
	 (destructuring-bind (e1 e2) pair
	   (when (subset-effect-p e1 e2)
	     (push (list e2 e1 (%indices e2 e1) :positive)
		   pairs))))
       effects
       :length 2)
      pairs)))

@export
(defun %release-candidates (effects ~effects)
  "Try to find negations of mutices, that is, those predicates which
represent resources being released."
  (let (acc)
    (map-product
     (lambda (e1 e2)
       (when (subset-effect-p e1 e2)
	 (push (list e2 e1 (%indices e2 e1) :negative)
	       acc)))
     effects ~effects)
    acc))

@export
(defun %indices (predicate mutex)
  (iter (for p in (parameters mutex))
	(collect (position p (parameters predicate)))))

(defun %validate-mutex (mutex checked-against)
  (ematch mutex
    ((or (list owner mutex indices kind)
	 (list owner mutex indices kind :validated))
     (if (every
	  (lambda (true-owner)
	    (some
	     (lambda (pred)
	       (and (eqname pred mutex)
		    (every #'pddl-supertype-p
		    	   (mapcar #'type (parameters pred))
		    	   (mapcar #'type (parameters mutex)))
		    (every
		     (lambda (param index)
		       (eq param (nth index (parameters true-owner))))
		     (parameters pred)
		     indices)))
	     checked-against))
	  (remove-if-not
	   (lambda (pred)
	     (and (eqname pred owner)
		  (every #'pddl-supertype-p
		  	 (mapcar #'type (parameters pred))
		  	 (mapcar #'type (parameters owner)))))
	   checked-against))
	 (list owner mutex indices kind :validated)
	 (list owner mutex indices kind :infeasible)))
    ((list _ _ _ _ :infeasible)
     mutex)))

(defun %validate-release (mutex checked-against ~checked-against)
  (ematch mutex
    ((or (list owner mutex indices kind)
	 (list owner mutex indices kind :validated))
     (if (every
	  (lambda (true-owner)
	    (some
	     (lambda (pred)
	       (and (eqname pred mutex)
		    (every #'pddl-supertype-p
		    	   (mapcar #'type (parameters pred))
		    	   (mapcar #'type (parameters mutex)))
		    (every
		     (lambda (param index)
		       (eq param (nth index (parameters true-owner))))
		     (parameters pred)
		     indices)))
	     ~checked-against))
	  (remove-if-not
	   (lambda (pred)
	     (and (eqname pred owner)
		  (every #'pddl-supertype-p
		  	 (mapcar #'type (parameters pred))
		  	 (mapcar #'type (parameters owner)))))
	   checked-against))
	 (list owner mutex indices kind :validated)
	 (list owner mutex indices kind :infeasible)))
    ((list _ _ _ _ :infeasible)
     mutex)))
   


@export
(defun categorize-mutices (candidates)
  (let ((hash (make-hash-table :test #'equalp)))
    (dolist (candidate candidates hash)
      (match candidate
	((list* owner mutex indices kind _)
	 (push candidate
	       (gethash (list (name owner)
			      (name mutex)
			      indices kind) hash)))))))

@export
(defun shrink-mutices (candidates-hash)
  (let (acc)
    (maphash
     (lambda (key mutices)
       @ignore key
       (when (every (lambda (mutex)
		      (eq :validated (fifth mutex))) mutices)
	 (push (car mutices) acc)))
     candidates-hash)
    acc))

