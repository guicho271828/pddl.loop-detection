
(in-package :pddl.loop-detection)
(use-syntax :annot)

;;;; mutex detection
;;;; primary function: mutex predicates

@export
(defun mutex-predicates (*domain*)
  "Returns two values.
The first value is a list of owner-release-mapping.
The second value is a list of owner-lock-mapping."
  (multiple-value-bind (owrs owls) (valid-owner-locks-in *domain*)
    (values
     (shrink-owner-locks
      (categorize-owner-locks owrs))
     (shrink-owner-locks
      (categorize-owner-locks owls)))))


;;;; small functions

@export
(defun subset-effect-p (sub super)
  "The basic function which checks if the arguments of effect1 is a subset of
effect2. Subset-ness is checked by eql (effect of the same name is instantiated
only once, this is ensured by the parser.)"
  (subsetp (parameters sub) (parameters super)))

@export
(defun index-mapping (sub super)
  "returns the parameter index mapping from `sub' to `super'."
  (iter (for p in (parameters sub))
        (collect (position p (parameters super)))))

@export
(defun specializes (e1 e2 &optional mapping)
  "Returns t when the parameters of e1 specializes e2
 (types of e1 is more strict than that of e2).
The optional third argument `mapping', when give,
is a list of indices which specifies the mapping from e2 to e1."
  (every #'pddl-supertype-p
         (mapcar #'type (if mapping
                            (mapcar (curry #'elt (parameters e1))
                                    mapping)
                            (parameters e1)))
         (mapcar #'type (parameters e2))))


;;;; owner-lock structure object
@export
(defstruct (owner-lock (:constructor owner-lock (owner lock mapping)))
  owner lock mapping)

(export '(owner-lock-p owner-lock-owner owner-lock-lock owner-lock-mapping))

(defpattern owner-lock (&optional (owner '_) (lock '_) (mapping '_))
  `(structure owner-lock- (owner ,owner) (lock ,lock) (mapping ,mapping)))

(defun owner->lock (owner-lock owner)
  (ematch owner
    ((pddl-predicate parameters)
     (ematch owner-lock
       ((owner-lock _ (pddl-predicate name) m)
        (pddl-atomic-state
         :name name
         :parameters (mapcar (lambda (n) (nth n parameters)) m)))))))

(defun acquire-or-release-p (owner-lock action)
  (match owner-lock
    ((owner-lock owner)
     (or (some (rcurry #'specializes owner)
               (add-list action))
         (some (rcurry #'specializes owner)
               (delete-list action))))))

;;;; subsuming-effects-in action
@export
(defun subsuming-effects-in (action)
  "For an action, returns a list of all pairs of effects that one maps to
and specializes the other. Returns four values: owner-releasers(occupying),
owner-releasers (releasing), owner-locks(occupying),
owner-locks (releasing).  owner-locks are meaningful only
when :negative-precondition is activated."
  (with-accessors ((a add-list)
                   (d delete-list)) action
    (values
     (find-subsuming-pairs a d)
     (find-subsuming-pairs d a)
     (find-subsuming-pairs a a)
     (find-subsuming-pairs d d))))


(defun maps-and-specializes-p (owner lock)
  "the mapping exists and owner specializes locks"
  (when (and (not (eqstate owner lock))
             (subset-effect-p lock owner))
    (let ((mapping (index-mapping lock owner)))
      (when (specializes owner lock mapping)
        mapping))))

(defun find-subsuming-pairs (maybe-owners maybe-locks)
  "For each owner o in maybe-owners, find m in maybe-locks that
satisfies the parameter subsumption condition."
  (let (acc)
    (dolist (owner maybe-owners acc)
      (dolist (lock maybe-locks)
        (when-let ((mapping (maps-and-specializes-p owner lock)))
          (push (owner-lock owner lock mapping) acc))))))

;;;; validate

(defmacro implies (a b)
  `(if ,a ,b t))

@export
(defun owner-releaser-valid-p (owner-lock action)
  (ematch owner-lock
    ((owner-lock owner)
     (or (every
          (lambda (o)
            (implies
             (specializes o owner)
             (let ((l (owner->lock owner-lock o)))
               (and
                (find l (positive-preconditions action) :test #'eqstate)
                (find l (delete-list action) :test #'eqstate)))))
          (add-list action))
         (every
           (lambda (o)
             (implies
              (specializes o owner)
              (let ((l (owner->lock owner-lock o)))
                (find l (add-list action) :test #'eqstate))))
           (delete-list action))))))

@export
(defun owner-lock-valid-p (owner-lock action)
  (ematch owner-lock
    ((owner-lock owner)
     (or (every
          (lambda (o)
            (implies
             (specializes o owner)
             (let ((l (owner->lock owner-lock o)))
               (and
                (find l (negative-preconditions action) :test #'eqstate)
                (find l (add-list action) :test #'eqstate)))))
          (add-list action))
         (every
           (lambda (o)
             (implies
              (specializes o owner)
              (let ((l (owner->lock owner-lock o)))
                (find l (delete-list action) :test #'eqstate))))
           (delete-list action))))))




;;;; collect everything among domain

(defun valid-owner-locks-in (domain)
  (let ((actions (actions domain)))
    (flet ((rvalid (owl)
             (every (curry #'owner-releaser-valid-p owl) actions))
           (ovalid (owl)
             (every (curry #'owner-lock-valid-p owl) actions)))
    (iter (for a in actions)
          (multiple-value-bind (ra rr oa or) (subsuming-effects-in a)
            (nconcing (remove-if-not #'rvalid ra) into owrs)
            (nconcing (remove-if-not #'rvalid rr) into owrs)
            (nconcing (remove-if-not #'ovalid oa) into owls)
            (nconcing (remove-if-not #'ovalid or) into owls))
          (finally (return (values owrs owls)))))))


(defun categorize-owner-locks (owner-locks)
  (categorize owner-locks
              :test #'equal
              :key (lambda (owner-lock)
                     (ematch owner-lock
                       ((owner-lock (pddl-predicate :name oname)
                                    (pddl-predicate :name lname) m)
                        (list oname lname m))))))

;;;; shrink the duplicated owners & locks

(defun choose-if (fn args1 args2 &key (key #'identity))
  (mapcar
   (lambda (a1 a2)
     (if (funcall fn
                  (funcall key a1)
                  (funcall key a2))
         a1 a2))
   args1 args2))


(defun merge-owners (owl1 owl2)
  (ematch owl1
    ((owner-lock
      (pddl-predicate :name oname :parameters oparam1)
      (pddl-predicate :name lname :parameters lparam1)
      m)
     (ematch owl2
       ((owner-lock
         (pddl-predicate :parameters oparam2)
         (pddl-predicate :parameters lparam2))
        (owner-lock
         (pddl-predicate
          :name oname
          :parameters (choose-if #'pddl-supertype-p
                                 oparam1 oparam2
                                 :key #'type))
         (pddl-predicate
          :name lname
          :parameters (choose-if #'pddl-supertype-p
                                 lparam1 lparam2
                                 :key #'type))
          m))))))

(defun shrink-owner-locks (hash)
  (let (acc)
    (maphash
     (lambda (key owner-locks)
       @ignore key
       (push (reduce #'merge-owners owner-locks) acc))
     hash)
    acc))

