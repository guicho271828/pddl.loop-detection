(in-package :pddl.loop-detection)
(use-syntax :annot)

;;;; utility

@export
(define-condition steady-state-condition (error)
  ((steady-state :accessor steady-state :initarg :steady-state)))

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

  (declaim (inline mutex-focused-planning))
  (declaim (ftype (function (list cons &key (:verbose boolean))
                            (function ((function (list list)
                                                 (values list list)))
                                      (or list keyword)))
                  mutex-focused-planning))

  @export
  (defun mutex-focused-planning
      (movements steady-state-tree &key (verbose t))
    (let* ((m-num (length movements))
           (buckets (make-array m-num :initial-element nil))
           (tree steady-state-tree)
           (ss nil))
      (named-lambda rec (branch-update-method)
        (multiple-value-setq (ss tree)
          (funcall branch-update-method ss tree))
        (let ((len (1- (length ss))))
          (more-labels () (%print-open %check-duplicates %fail)
            (%print-open)
            (%check-duplicates)
            (when-let ((results (search-loop-path movements ss)))
              (nconcf (aref buckets len) results)
              (return-from rec results))
            (%fail)))))))

