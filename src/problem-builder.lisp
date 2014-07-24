(in-package :pddl.loop-detection)
(use-syntax :annot)

;;;; utility

(defun obj (obj n)
  (intern (format nil "~a~a" (name obj) n)))

(defun states-only (states)
  ;; remove function-states and takes only atomic-states
  (remove-if-not (rcurry #'typep 'pddl-atomic-state) states))

(defun instance-p (predicate atomic-state)
  (and (eqname atomic-state predicate)
       (specializes atomic-state predicate)))

;;;; main description

#|

problem : pddl problem
example : ss = (0 1 2 9)
schedule : list of timed actions, must be sorted
movements : list of (movement index resources)
component : list of objects. they should (?) form a static component
static-facts. It statically connects the component.

XXX `static-facts' is nessesary (? TODO ?) when there are multiple objects in the
component because the new objects (corresponding to each step of the mfp)
should also be connected.
XXX this is not the case because the function already captures all facts
related to each object.

|#

;;;; sub-functions
(progn
;;;; objects
  (define-local-function %loop-objects ()
    (append
     (env-objects objects component)
     (iter outer
           (for i in ss)
           (iter (for c in component)
                 (in outer
                     (collect
                         (shallow-copy c :name (obj c i))))))))

  (defun env-objects (objs component)
    (iter (for o in objs)
          (when (iter (for c in component)
                      (never (pddl-supertype-p (type o) (type c))))
            (collect o))))

;;;; inits and goals

  (define-local-function %loop-inits ()
    (append
     (%environment)
     (mappend #'%component-states ss)
     (mappend #'%releaser-states owner-releasers)))
  (define-local-function %loop-goals ()
    (append
     (states-only (%environment)) ; remove the function-state
     (mappend #'%component-states (make-eol ss (length ss)))
     (mappend #'%releaser-states owner-releasers)))

  (define-local-function %environment ()
    "Returns The global states, that is, any states which is not describing the state
of any components and is not describing the lock state. (owner has
components in its arguments, so this is safe)"
    (remove-if
     (lambda (state)
       (match state
         ((pddl-function-state) state)
         ((pddl-atomic-state)
          (or
           ;; removes the state of a component.
           ;; owners are always contained in it.
           (some (rcurry #'related-to state) component)
           ;; Next, remove the locks and releasers.  Removing those states
           ;; is safe because they are added later by %component-states and
           ;; %releaser-states.
           (some (lambda-match ((owner-lock _ l _)
                                (instance-p l state)))
                 owner-locks)
           (some (lambda-match ((owner-lock _ l _)
                                (instance-p l state)))
                 owner-releasers)))))
     init))

  (define-local-function %component-states (i)
    "The states which describes the component in the
   particular step in the unit plan, replacing the base
   with a new object in the steady-state."
    (iter
      (for p in (%prototypes i))
      (collect (shallow-copy p :parameters
                             (iter (for o in (parameters p))
                                   (collect (or (find o component) o)))))
      (when-let ((owl (some (lambda-match
                              ((owner-lock o)
                               (instance-p p o)))
                            owner-locks)))
        ;; if p matches to an owner, then add the lock simultaneously
        (collect
            (match owl
              ((owner-lock _ (pddl-predicate name) m)
               (pddl-atomic-state
                :name name
                :parameters (mapcar (curry #'elt (parameters p)) m))))))))

  (define-local-function %prototypes (index)
    (remove-if-not
     (lambda (atomic-state)
       (some (rcurry #'related-to atomic-state) component))
     (timed-state-state
      (timed-action-end
       (nth (movement-index (nth index movements)) schedule)))))

  (define-local-function %releaser-states (owner-releaser)
    (match owner-releaser
      ((owner-lock o r)
       (when-let ((releasers (remove-if-not (rcurry #'instance-p r) init)))
         (when (iter outer
                     (for i in ss)
                     (iter (for p in (%prototypes i))
                           (in outer
                               (never (instance-p o p)))))
           releasers)))))


;;;; main code
  (defun build-loop-problem (problem ss schedule movements &rest component)
    (ematch problem
      ((pddl-problem name domain objects init)
       (let ((*domain* domain))
         (iter (for o in component) (assert (find o objects)))
         (multiple-value-bind (owner-releasers owner-locks)
             (mutex-predicates *domain*)
           ;; (let* ((env-objects (set-difference objects loop-objects))
           ;;        (init (categorize 
           ;;               init :key 
           ;;               (lambda (state)
           ;;                 (if (some (rcurry #'related-to state) bases) t nil))))
           ;;        (init/bases (gethash nil init))
           ;;        (mutices (mutex-predicates *domain*)))
           ;; NOTE!! the PROBLEM slot of each object still refers to the old
           ;; problem!
           (more-labels () (%loop-objects
                            %loop-inits
                            %loop-goals
                            %environment
                            %component-states
                            %prototypes
                            %releaser-states)
             (shallow-copy
              problem
              :name (apply #'concatenate-symbols name 'ss ss)
              :objects (%loop-objects)
              :init (%loop-inits)
              :goal (%loop-goals)))))))))

