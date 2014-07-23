(in-package :pddl.loop-detection)
(use-syntax :annot)

(defun make-buckets (n)
  (make-array n :initial-element nil))

(defun %forward-duplication-check (ss end bucket)
  (some
   (lambda (loop-path)
     (or (find ss loop-path :test #'equalp)
         (find end loop-path :test #'equalp)))
   bucket))

(defun %post-duplication-check (buckets)
  (map 'vector #'%post-duplication-check/bucket buckets))
(defvar *flag*)
(defun %post-duplication-check/bucket (bucket)
  (let ((*flag* nil))
    (values (remove-duplicates bucket :test #'%loop-equal)
            *flag*)))
(defun %loop-equal (path1 path2)
  (when-let ((it (or (find (first-elt path1) path2 :test #'equalp)
                     (find (last-elt path1) path2 :test #'equalp)
                     (find (first-elt path2) path1 :test #'equalp)
                     (find (last-elt path2) path1 :test #'equalp))))
    (setf *flag* t)
    it))

(defun %constraint< (ss1 ss2)
  (subsetp ss1 ss2 :test #'=))

(defun %fast-failure-check (ss bucket)
  (some (rcurry #'%constraint< ss) bucket))

(declaim (inline funcall-if-functionp))
(defun funcall-if-functionp (fn &rest arguments)
  (when (functionp fn)
    (apply fn arguments)))

@export
(define-condition steady-state-condition (error)
  ((steady-state :accessor steady-state :initarg :steady-state)))

(export '(steady-state skip-this wind-stack))

(progn
  (declaim (inline %loop-verbosity-lazy))
  (defun %loop-verbosity-lazy (forward-duplication-check-p
                               post-duplication-check-p
                               steady-state-tree ;; lazy tree -- to be refactored
                               moves ;; movements
                               verbose)
    (let* ((m-num (length moves))
           (it (tree-iterator steady-state-tree :lazy t))
           (buckets (make-buckets m-num))
           (bucket-locks (make-array m-num)))
      (dotimes (j m-num)
        (setf (aref bucket-locks j) (make-lock)))
      (label1 rec ()
          (handler-return
              ((tree-exhausted (lambda (c)
                                 (declare (ignore c))
                                 (values nil buckets))))
            (let (ss stack)
              (tagbody
               :start
                 (multiple-value-setq (ss stack) (funcall it))
                 (format t "~&Fetching ~a " ss)
                 (restart-bind
                     ((skip-this (lambda ()
                                   (format t " ... Skipping")
                                   (go :start)))
                      (wind-stack (lambda ()
                                    (format t " ... Winding")
                                    (funcall it :wind-stack stack)
                                    (go :start)))
                      (continue (lambda () (go :cont))))
                   (error 'steady-state-condition :steady-state ss))
                 
               :cont
                 
                 (let* ((bases (1- (length ss))))
                   ;; forward-duplication-check
                   (when (and forward-duplication-check-p
                              (%forward-duplication-check
                               ss
                               (make-eol ss m-num)
                               (aref buckets bases)))
                     (format t " ... Duplicated")
                     (go :start))
                   
                   (when-let ((results
                               (search-loop-path
                                moves ss :verbose verbose)))
                     ;; success
                     (let (new-bucket duplicated?)
                       (with-lock-held ((aref bucket-locks bases))
                         (let ((bucket (nconc results (aref buckets bases))))
                           (multiple-value-setq (new-bucket duplicated?)
                             (if post-duplication-check-p
                                 (%post-duplication-check/bucket bucket)
                                 bucket))
                           (setf (aref buckets bases) new-bucket)))
                       (if duplicated?
                           (go :start)
                           (return-from rec
                             (values (cons results #'rec) buckets it))))))
                 
                 ;; failure
                 (format t " ... No loop")
                 (funcall it :wind-stack stack)
                 (go :start))))
        #'rec)))

  (defun %none-lazy (moves steady-state-tree
                     forward-duplication-check-p
                     post-duplication-check-p)
    (%loop-verbosity-lazy forward-duplication-check-p
                          post-duplication-check-p
                          steady-state-tree
                          moves
                          nil))

  @export
  (defun exploit-loopable-steady-state-lazy
      (movements-shrinked steady-state-tree
       &key (verbose t) (duplication-check t))
    "Returns the list of solution path from start-of-loop (MS3) to
end-of-loop (MS3)."
    (declare (ignorable verbose))
    (declare (optimize (speed 3)))
    (declare (inline %none-lazy))
    (macrolet ((%call (name)
                 `(case duplication-check
                    ((nil)           (,name movements-shrinked steady-state-tree nil nil))
                    (:post-only    (,name movements-shrinked steady-state-tree nil t))
                    (:forward-only (,name movements-shrinked steady-state-tree t nil))
                    (otherwise     (,name movements-shrinked steady-state-tree t t)))))
      (%call %none-lazy))))
