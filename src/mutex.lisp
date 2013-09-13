
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
	     (actions domain))
      (mutices-in a)))
   (actions domain)))

@export
(defun %validate-mutex-among-actions (actions mutex)
  (ematch mutex
    ((list _ _ _ :positive _)
     (dolist (a2 actions)
       (setf mutex (%validate mutex (delete-list a2) (delete-list a2)))
       (setf mutex (%validate mutex (add-list a2) (add-list a2)))
       (setf mutex (%validate mutex (add-list a2) (negative-preconditions a2)))
       ))
    ((list _ _ _ :negative _)
     (dolist (a2 actions)
       (setf mutex (%validate mutex (add-list a2) (delete-list a2)))
       (setf mutex (%validate mutex (delete-list a2) (add-list a2)))
       (setf mutex (%validate mutex (add-list a2) (positive-preconditions a2))))))
  mutex)


@export
(defun mutices-in (action)
  (with-accessors ((a add-list) (d delete-list)) action
    (append
      (mapcar (rcurry #'%validate d d) (%candidates a a :positive))
      (mapcar (rcurry #'%validate a a) (%candidates d d :positive))
      (mapcar (rcurry #'%validate d a) (%candidates a d :negative))
      (mapcar (rcurry #'%validate a d) (%candidates d a :negative)))))

@export
(defun %candidates (maybe-owners maybe-mutices kind)
  "Try to find negations of mutices, that is, those predicates which
represent resources being released."
  (let (acc)
    (map-product
     (lambda (e1 e2)
       (when (and (not (eq e1 e2))
                  (subset-effect-p e1 e2))
	 (push (list e2 e1 (%indices e2 e1) kind)
	       acc)))
     maybe-owners maybe-mutices)
    acc))

@export
(defun %indices (predicate mutex)
  (iter (for p in (parameters mutex))
	(collect (position p (parameters predicate)))))

(defun %matches-to-owner-p (owner pred)
  (and (eqname pred owner)
       (every #'pddl-supertype-p
	      (mapcar #'type (parameters pred))
	      (mapcar #'type (parameters owner)))))

(defun %matches-to-mutex-p (mutex indices true-owner pred)
  (and (eqname pred mutex)
       (every #'pddl-supertype-p
	      (mapcar #'type (parameters pred))
	      (mapcar #'type (parameters mutex)))
       (every
	(lambda (param index)
	  (eq param (nth index (parameters true-owner))))
	(parameters pred)
	indices)))

(defun %owner-implies-mutex-p (owner mutex indices maybe-owners maybe-mutices)
  (every
   (lambda (true-owner)
     (some
      (curry #'%matches-to-mutex-p
             mutex indices true-owner)
      maybe-mutices))
   (remove-if-not
    (curry #'%matches-to-owner-p owner)
    maybe-owners)))

(defun %validate (mutex maybe-owners maybe-mutices)
  (ematch mutex
    ((or (list owner mutex indices kind)
	 (list owner mutex indices kind :validated))
     (if (%owner-implies-mutex-p  owner mutex indices maybe-owners maybe-mutices)
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
			      (mapcar #'type (parameters owner))
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

