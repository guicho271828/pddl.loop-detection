
(in-package :pddl.loop-detection)
(use-syntax :annot)

@export
(defun related-actions (predicate)
  "returns a list of actions which uses the specified predicate in
its precondition or the effect."
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

(defmethod related-to (designator (ta timed-action))
  (related-to designator (timed-action-action ta)))

;; type of mutex??
@export
(defun acquire-or-release-mutex-p (mutex action)
  (match mutex
    ((list* owner _)
     (or (some (curry #'predicate-more-specific-p owner)
               (add-list action))
         (some (curry #'predicate-more-specific-p owner)
               (delete-list action))))))

@export
(defun %extract-movements (object schedule domain)
  (let (related indices)
    (iter (for ta in schedule)
          (for i from 0)
          (when (related-to object ta)
            (collect ta into %related)
            (collect i into %indices))
          (finally (setf related %related
                         indices %indices)))
    (values
     (mapcar (compose
              (let ((owners (mapcar #'first (mutex-predicates domain))))
                (lambda (states)
                  (remove-if-not
                   (lambda (atomic-state)
                     (ematch atomic-state
                       ((type pddl-atomic-state)
                        (some
                         (curry #'predicate-more-specific-p atomic-state)
                         owners))
                       ((type pddl-function-state)
                        t)))
                   states)))
              (lambda (ta)
                (match ta
                  ((timed-action _ _ _ (timed-state _ states _))
                   (remove-if-not (curry #'related-to object)
                                  states)))))
             related)
     indices)))

@export
(defun shrink-movements (movements indices)
  (iter (for states in (cdr movements))
        (for i in (cdr indices))
        (for pstates in movements)
        (for previ in indices)
        (unless (set-equal states pstates :test #'eqstate)
          (collect pstates into shrinked-states)
          (collect previ into shrinked-state-indices))
        (finally
         (return (values
                  shrinked-states
                  shrinked-state-indices)))))


@export
(defun extract-movements (object schedule domain)
  (multiple-value-call
      #'shrink-movements
    (%extract-movements object schedule domain)))
