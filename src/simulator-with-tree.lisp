(in-package :pddl.loop-detection)
(use-syntax :annot)

(progn
  (declaim (inline %loop-verbosity-lazy))
  (defun %loop-verbosity-lazy (before
                               after-success
                               after-failure
                               after-duplication
                               forward-duplication-check-p
                               post-duplication-check-p
                               steady-state-tree
                               moves
                               verbose)
    (let* ((m-num (length moves))
           (it (tree-iterator steady-state-tree :lazy t))
           (buckets (make-buckets m-num))
           (bucket-locks (make-array m-num)))
      (dotimes (j m-num)
        (setf (aref bucket-locks j) (make-lock)))
      (label1 rec ()
          (handler-return
              ((tree-exhausted (lambda (c) (declare (ignore c)) buckets)))
            (multiple-value-bind (ss stack) (funcall it)
              (let* ((bases (1- (length ss))))
                (funcall-if-functionp before ss)
                (cond
                  ;; forward-duplication-check
                  ((and forward-duplication-check-p
                        (%forward-duplication-check
                         ss (make-eol ss m-num) (aref buckets bases)))
                   (funcall-if-functionp after-duplication ss)
                   (rec)) ; next iteration
                  
                  (t
                   (if-let ((results
                             (search-loop-path
                              moves ss :verbose verbose)))
                     ;; success
                     (progn
                       (funcall-if-functionp after-success ss results)
                       (with-lock-held ((aref bucket-locks bases))
                         (let ((bucket (nconc results (aref buckets bases))))
                           (multiple-value-bind (new-bucket duplicated?)
                               (if post-duplication-check-p
                                   (%post-duplication-check/bucket bucket)
                                   bucket)
                             (setf (aref buckets bases) new-bucket)
                             (if duplicated?
                                 (rec)
                                 (values (cons results #'rec) buckets it))))))
                     (progn ; failure
                       (funcall-if-functionp after-failure ss)
                       (funcall it :wind-stack stack)
                       (rec))))))))
        #'rec)))

  (defun %none-lazy (moves steady-state-tree
                     forward-duplication-check-p
                     post-duplication-check-p)
    (%loop-verbosity-lazy nil nil nil nil
                          forward-duplication-check-p
                          post-duplication-check-p
                          steady-state-tree
                          moves
                          nil))

  @export
  (defun exploit-loopable-steady-state-lazy (movements-shrinked steady-state-tree
                                             &key (verbose t) (duplication-check t))
    "Returns the list of solution path from start-of-loop to end-of-loop.
start-of-loop is always the same list to the steady-state in the
meaning of EQUALP."
    (declare (ignorable verbose))
    (declare (optimize (speed 3)))
    (declare (inline %none-lazy))
    (macrolet ((%call (name) `(case duplication-check
                                ((nil)           (,name movements-shrinked steady-state-tree nil nil))
                                (:post-only    (,name movements-shrinked steady-state-tree nil t))
                                (:forward-only (,name movements-shrinked steady-state-tree t nil))
                                (otherwise     (,name movements-shrinked steady-state-tree t t)))))
      (%call %none-lazy))))
