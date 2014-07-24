(in-package :pddl.loop-detection)
(use-syntax :annot)

(defstruct (bfs-state (:constructor bfs-state (current ltree))
                      (:predicate bfs-state-p))
  current
  ltree)

@export
(defun best-first-mfp (movements &key verbose)
  (let ((searcher (mfp-with-filtering movements :verbose verbose)))
    (labels ((eval-branch (current successors open)
               (cons (funcall searcher current)
                     (lambda (real-cost)
                       (print successors)
                       (open-minimum
                        (append-queue real-cost successors open)))))
             (open-minimum (open)
               (multiple-value-bind (popped open) (pop-queue-minimum open)
                 (ematch popped
                   ((bfs-state current ltree)
                    (let ((tree (fmapcar #'identity ltree)))
                      (eval-branch (cons (car tree) current)
                                   (cdr tree)
                                   open)))))))
      (open-minimum
       (insert-queue
        MOST-NEGATIVE-FIXNUM
        (bfs-state nil (steady-state movements))
        (make-queue))))))