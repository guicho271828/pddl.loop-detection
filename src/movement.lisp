
(in-package :pddl.loop-detection)
(use-syntax :annot)

;;;; per-action

(defun resource-p (object owner-lock atomic-state)
  (and (find object (parameters atomic-state))
       (ematch owner-lock
         ((owner-lock o)
          (specializes atomic-state o)))))

@export
(defun extract-resources (object sorted-schedule
                          &optional
                            (*problem* *problem*)
                            (*domain*
                             (or *domain* (domain *problem*))))
  (let ((owls (mutex-predicates *domain*))
        (object (object *problem* object)))
    (iter (for ta in sorted-schedule)
          (collect
              (match ta
                ((timed-action _ _ _ (timed-state _ states _))
                 (remove-if-not
                  (lambda (s)
                    (some (lambda (owl)
                            (resource-p object owl s))
                          owls))
                  states)))))))

@export
(defun extract-movements (object schedule
                          &optional
                            (*problem* *problem*)
                            (*domain*
                             (or *domain* (domain *problem*))))
  "Returns a list of (number . owners) in a schedule.
The given schedule should be sorted beforehand."
  (iter (for resources in
             (extract-resources object schedule))
        (for prev previous resources)
        (for i from 0)
        (when (or (zerop i)
                  (not (set-equal resources prev)))
          (collect (movement i resources)))))

@export
(defun movement (index resources)
  (cons index resources))
(defpattern movement (index resources)
  `(cons ,index ,resources))

@export
(defun movement-resources (movement)
  (cdr movement))
@export
(defun movement-index (movement)
  (car movement))


@export
(defun merge-movements (&rest more-movements)
  (if (null more-movements)
      nil
      (let ((hash (make-hash-table)))
        (flet ((accumulate (movements)
                 (iter (for m in movements)
                       (match m
                         ((movement index resources)
                          (setf (gethash index hash)
                                (union (gethash index hash)
                                       resources
                                       :test #'eqstate)))))))
          (mapc #'accumulate more-movements))
        (let (acc)
          (maphash (lambda (k v) (push (movement k v) acc)) hash)
          acc))))
