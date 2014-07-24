(in-package :pddl.loop-detection)
(use-syntax :annot)

;;;; main

(progn
  (define-local-function %print-open ()
    (when verbose (format t "~&Fetching ~a " ss)))
  (define-local-function %check-duplicates ()
    (when (some
           (lambda (loop-path)
             (or (find ss loop-path :test #'equal)
                 (find (make-eol ss len) loop-path :test #'equal)))
           (aref buckets len))
      (when verbose (format t " ... Duplicated"))
      (return-from rec :duplicated)))

  (define-local-function %fail ()
    (when verbose (format t " ... No loop"))
    (return-from rec :fail))

  (declaim (inline mfp-with-filtering))
  (declaim (ftype (function (list &key (:verbose boolean))
                            (function (list) (or keyword list)))
                  mfp-with-filtering))

  @export
  (defun mfp-with-filtering (movements &key verbose)
    (let* ((m-num (length movements))
           (buckets (make-array m-num :initial-element nil)))
      (named-lambda rec (ss) ; evaluation method
         (let ((len (1- (length ss))))
           (more-labels () (%print-open %check-duplicates %fail)
             (%print-open)
             (%check-duplicates)
             (when-let ((results (mutex-focused-planning movements ss)))
               (nconcf (aref buckets len) results)
               (return-from rec results))
             (%fail)))))))

(defstruct (bfs-state (:constructor bfs-state (current ltree))
                      (:predicate bfs-state-p))
  current
  ltree)

(defun best-first-mfp (movements &key verbose)
  (let ((searcher (mfp-with-filtering movements :verbose verbose)))
    (labels ((eval-branch (current tree open)
               (cons (funcall searcher current)
                     (lambda (real-cost)
                       (open-minimum
                        (insert-queue real-cost tree open)))))
             (open-minimum (open)
               (multiple-value-bind (open content)
                   (rb-remove-minimum-node open)
                 (match content
                   ((bfs-state current ltree)
                    (let ((tree (fmapcar #'identity ltree)))
                      (eval-branch (cons (car tree) current)
                                   tree
                                   open)))))))
      (open-minimum
       (insert-queue
        MOST-NEGATIVE-FIXNUM
        (bfs-state nil (steady-state movements))
        (make-queue))))))
