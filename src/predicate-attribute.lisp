
(in-package :pddl.loop-detection)
(use-syntax :annot)

@export
(defun related-actions (predicate)
  (match predicate
    ((pddl-predicate name domain)
     (remove-if-not
      (named-lambda per-action (action)
	(flet ((per-branch (branch cont)
		 (match branch
		   ((op _ preds) (funcall cont preds))
		   ((pddl-predicate :name name2)
		    (when (eq name name2)
		      (return-from per-action t))))))
	  (walk-tree #'per-branch (precondition action))
	  (walk-tree #'per-branch (effect action))
	  nil))
      (actions domain)))))

;; 以下、mutex検知

@export
(defun mutex-predicates (domain)
  (shrink-muteces
   (categorize-muteces
    (mappend #'muteces-in (actions domain)))))

@export
(defun muteces-in (action)
  (shrink-muteces
   (categorize-muteces
    (append (mutex-candidates-in (add-list action))
	    (mutex-candidates-in (delete-list action))))))


@export
(defun subset-effect-p (effect1 effect2)
  (subsetp (parameters effect1)
	   (parameters effect2)))

@export
(defun get-mutex-argument-indices (predicate mutex)
  (iter (for p in (parameters mutex))
	(collect
	    (position p (parameters predicate)))))

@export
(defun mutex-candidates-in (effects)
  (if (> (length effects) 1)
      (let (pairs)
	(map-permutations
	 (lambda (pair)
	   (when (apply #'subset-effect-p pair)
	     (destructuring-bind (mutex predicate) pair
	       (push (reverse 
		      (cons (get-mutex-argument-indices predicate mutex)
			    pair)) pairs))))
	 effects
	 :length 2)
	pairs)
      effects))

@export
(defun categorize-muteces (candidates)
  (let ((hash (make-hash-table :test #'equalp)))
    (dolist (candidate candidates hash)
      (match candidate
	((list (pddl-predicate :name pred-name)
	       (pddl-predicate :name mutex-name)
	       _)
	 (push candidate
	       (gethash (cons pred-name mutex-name) hash)))))))

@export
(defun shrink-muteces (candidates-hash)
  (let (muteces)
    (maphash (lambda (key value)
	       @ignore key
	       (let ((shrinked (remove-duplicates
				value
				:test #'equalp
				:key #'third)))
		 (when (= 1 (length shrinked))
		   (push (car shrinked) muteces))))
	     candidates-hash)
    muteces))

