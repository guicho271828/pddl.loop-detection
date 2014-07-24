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
               (let ((plan (funcall searcher (reverse current))))
                 (cons plan
                       (lambda (real-cost)
                         (when verbose
                           (format t "~&Plan: ~a, Given value : ~a"
                                   plan real-cost))
                         (open-minimum
                          (append-queue real-cost
                                        (mapcar (lambda (branch)
                                                  (bfs-state current branch))
                                                successors)
                                        open))))))
             (open-minimum (open)
               (multiple-value-bind (popped open) (pop-queue-minimum open)
                 (ematch popped
                   ((bfs-state current ltree)
                    (let ((tree (fmapcar #'identity ltree)))
                      (eval-branch (cons (car tree) current)
                                   (cdr tree)
                                   open)))
                   (nil ; exhausted
                    (warn "tree exhausted!"))))))
      (open-minimum
       (insert-queue
        MOST-NEGATIVE-FIXNUM
        (bfs-state nil (steady-state movements))
        (make-queue))))))
