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

(progn
  (declaim (inline funcall-if-functionp))
  (defun funcall-if-functionp (fn &rest arguments)
    (when (functionp fn)
      (apply fn arguments)))

  (declaim (inline %loop-verbosity))
  (defun %loop-verbosity (before
                          after-success
                          after-failure
                          after-duplication
                          forward-duplication-check-p
                          post-duplication-check-p
                          steady-states
                          moves
                          verbose)
    (let* ((m-num (length moves))
           (max (length steady-states))
           (buckets (make-buckets m-num))
           (bucket-locks (make-array m-num))
           (failure-buckets (make-buckets m-num))
           (failure-buckets-locks (make-buckets m-num)))
      @ignorable max
      (dotimes (j m-num)
        (setf (aref failure-buckets-locks j) (make-lock))
        (setf (aref bucket-locks j) (make-lock)))
      (flet ((%exploit/thread (ss)
               (let ((bases (1- (length ss))))
                 (funcall-if-functionp before)
                 (cond
                   ((and forward-duplication-check-p
                         (%forward-duplication-check ss
                                                     (make-eol ss m-num)
                                                     (aref buckets bases)))
                    (funcall-if-functionp after-duplication ss))
                   ((iter (for bucket in-vector failure-buckets below bases)
                          (thereis (%fast-failure-check ss bucket)))
                    (funcall-if-functionp after-failure))
                   (t
                    (if-let ((results
                              (search-loop-path
                               moves ss :verbose verbose)))
                      (progn
                        (funcall-if-functionp after-success results)
                        (with-lock-held ((aref bucket-locks bases))
                          (appendf (aref buckets bases) results)))
                      (progn
                        (funcall-if-functionp after-failure)
                        (with-lock-held ((aref failure-buckets-locks bases))
                          (push ss (aref failure-buckets bases)))
                        (iter (for bucket
                                   in-vector failure-buckets
                                   with-index j above bases)
                              (with-lock-held ((aref failure-buckets-locks j))
                                (setf (aref failure-buckets j)
                                      (remove-if (curry #'%constraint< ss)
                                                 (aref failure-buckets j))))))))))))
        (pmap nil
              #'%exploit/thread
              (shuffle (coerce steady-states 'vector)))
        (format *shared-output* "~&Main search finished. accumulating...")
        (reduce #'append (if post-duplication-check-p
                             (%post-duplication-check buckets)
                             buckets)
                :from-end t))))

  (defun %verbose (moves steady-states
                   forward-duplication-check-p
                   post-duplication-check-p)
    (let ((i 0)
          (i-lock (make-lock "i"))
          (max (length steady-states)))
      (%loop-verbosity
       (lambda ()
         (with-lock-held (i-lock)
           (incf i)
           (with-lock-held (*print-lock*)
             (format *shared-output* "~%~a/~a: " i max))))
       nil
       nil
       (lambda (ss)
         (with-lock-held (*print-lock*)
           (format *shared-output*
                   "~w is not searched because it had appeared in the other loop."
                   ss)))
       forward-duplication-check-p
       post-duplication-check-p
       steady-states
       moves
       t)))

  (defun %modest (moves steady-states
                  forward-duplication-check-p
                  post-duplication-check-p)
    (let ((buffer (make-array 61
                              :element-type 'character
                              :initial-element #\Space
                              :fill-pointer 0)))
      (%loop-verbosity
       (lambda ()
         (with-lock-held (*print-lock*)
           (when (= (fill-pointer buffer) 59)
             (loop for c across buffer
                do (write-char c *shared-output*))
             (terpri *shared-output*)
             (setf (fill-pointer buffer) 0))))
       (lambda (results)
         (with-lock-held (*print-lock*)
           (vector-push-extend (density-char (length results)) buffer)))
       (lambda ()
         (with-lock-held (*print-lock*)
           (vector-push-extend #\Space buffer)))
       (lambda (ss)
         @ignore ss
         (with-lock-held (*print-lock*)
           (vector-push-extend #\D buffer)))
       forward-duplication-check-p
       post-duplication-check-p
       steady-states
       moves
       nil)))

  (defun %none (moves steady-states
                forward-duplication-check-p
                post-duplication-check-p)
    (%loop-verbosity nil nil nil nil
                     forward-duplication-check-p
                     post-duplication-check-p
                     steady-states
                     moves
                     nil))

  @export
  (defun exploit-loopable-steady-states (movements-shrinked steady-states
                                         &key (verbose t) (duplication-check t))
    "Returns the list of solution path from start-of-loop to end-of-loop.
start-of-loop is always the same list to the steady-state in the
meaning of EQUALP."
    (declare (optimize (speed 3)))
    (declare (inline %verbose
                     %modest
                     %none))
    (macrolet ((%call (name) `(case duplication-check
                                ((nil)           (,name movements-shrinked steady-states nil nil))
                                (:post-only    (,name movements-shrinked steady-states nil t))
                                (:forward-only (,name movements-shrinked steady-states t nil))
                                (otherwise     (,name movements-shrinked steady-states t t)))))
      (case verbose
        ((2 t)       (%call %verbose))
        ((1 :modest) (%call %modest))
        (otherwise   (%call %none))))))
