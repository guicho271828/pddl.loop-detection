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


