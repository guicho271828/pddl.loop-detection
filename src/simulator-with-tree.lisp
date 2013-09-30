(in-package :pddl.loop-detection)
(use-syntax :annot)


#|

 (0 (1 2 3))

iterator yields:

 (0)
 (0 1)
 (0 1 2)
 (0 1 3)

|#

(define-condition tree-exhausted (condition)
  ())

@export
@doc "depth first tree iterator"
(defun tree-iterator (tree)
  (let ((first t)
        env
        stack
        (lock (make-lock "Tree lock")))
    (lambda (&key wind-stack get-current-stack)
      (with-lock-held (lock)
        (cond
          (get-current-stack
           stack)
          (wind-stack
           ;; (format t "~&winding stack until ~w" wind-stack)
           (if (find wind-stack stack)
               (iter (until (eql wind-stack (car stack)))
                     ;; (format t "~&~4twinding ~w" (car stack))
                     (pop stack)
                     (pop env)
                     (finally
                      ;; (format t "~&~4twinding ~w" (car stack))
                      (pop env)
                      (pop stack)
                      (return (car stack))))
               ;; (format t "~& stack ~w is not found!" wind-stack)
               ))
          
          ((and (null env) (null stack) first)
           ;; first time
           (push tree stack)
           (push (cdr tree) env)
           (setf first nil)
           (values (list (first tree))
                   (car stack)))
          ((and (null env) (null stack))
           (signal 'tree-exhausted))
          
          ((and env (first env) (consp (first env)) stack)
           (let ((opened (pop (first env))))
             (multiple-value-prog1
                 (cond
                   ((atom opened)
                    (values 
                     (nreverse (cons opened (mapcar #'first stack)))
                     (car stack)))
                   ((consp opened)
                    (push opened stack)
                    (push (cdr opened) env)
                    (values 
                     (nreverse (mapcar #'first stack))
                     (car stack))))
               (iter (while (and env (null (first env))))
                     ;; (sleep 0.2)
                     ;; (format t "~& ~w ~w" env stack)
                     (pop env)
                     (pop stack)))))
          
          ((and env (first env) (atom (first env)) stack)
           ;; backtrack
           (multiple-value-prog1
               (values 
                (nreverse (cons (first env) (mapcar #'first stack)))
                (car stack))
             (pop env)))
          (t (error "no match ~w ~w" env stack)))))))


(progn
  (declaim (inline %loop-verbosity-tree))
  (defun %loop-verbosity-tree (before
                               after-success
                               after-failure
                               after-duplication
                               forward-duplication-check-p
                               post-duplication-check-p
                               steady-state-tree
                               moves
                               verbose)
    (let* ((m-num (length moves))
           (it (tree-iterator steady-state-tree))
           (buckets (make-buckets m-num))
           (bucket-locks (make-array m-num))
           (channel (make-channel)))
      (dotimes (j m-num)
        (setf (aref bucket-locks j) (make-lock)))
      
      (labels ((%exploit/thread ()
                 (handler-return ((tree-exhausted
                                   (lambda (c)
                                     @ignore c
                                     nil)))
                   (multiple-value-bind (ss stack) (funcall it)
                     (let* ((bases (1- (length ss))))
                       (funcall-if-functionp before)
                       (cond
                         ;; forward-duplication-check
                         ((and forward-duplication-check-p
                               (%forward-duplication-check ss
                                                           (make-eol ss m-num)
                                                           (aref buckets bases)))
                          (funcall-if-functionp after-duplication ss))
                         
                         (t
                          (if-let ((results
                                    (search-loop-path
                                     moves ss :verbose verbose)))
                            (progn ; success
                              (funcall-if-functionp after-success results)
                              (with-lock-held ((aref bucket-locks bases))
                                (appendf (aref buckets bases) results)))
                            (progn ; failure
                              (funcall-if-functionp after-failure)
                              (funcall it :wind-stack stack)))))))
                   (submit-task channel #'%exploit/thread)
                   t)))
        
        (dotimes (i (kernel-worker-count))
          (submit-task channel #'%exploit/thread))
        
        (iter (while (receive-result channel)))
          
        (format *shared-output* "~&Main search finished. accumulating...")
        (reduce #'append (if post-duplication-check-p
                             (%post-duplication-check buckets)
                             buckets)
                :from-end t))))

  (defun %verbose-tree (moves steady-state-tree
                        forward-duplication-check-p
                        post-duplication-check-p)
    (let ((i 0)
          (i-lock (make-lock "i"))
          (max (length steady-state-tree)))
      (%loop-verbosity-tree
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
       steady-state-tree
       moves
       t)))

  (defun %modest-tree (moves steady-state-tree
                       forward-duplication-check-p
                       post-duplication-check-p)
    (let ((buffer (make-array 61
                              :element-type 'character
                              :initial-element #\Space
                              :fill-pointer 0)))
      (%loop-verbosity-tree
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
       steady-state-tree
       moves
       nil)))

  (defun %none-tree (moves steady-state-tree
                     forward-duplication-check-p
                     post-duplication-check-p)
    (%loop-verbosity-tree nil nil nil nil
                          forward-duplication-check-p
                          post-duplication-check-p
                          steady-state-tree
                          moves
                          nil))

  @export
  (defun exploit-loopable-steady-state-tree (movements-shrinked steady-state-tree
                                             &key (verbose t) (duplication-check t))
    "Returns the list of solution path from start-of-loop to end-of-loop.
start-of-loop is always the same list to the steady-state in the
meaning of EQUALP."
    (declare (optimize (speed 3)))
    (declare (inline %verbose
                     %modest
                     %none))
    (macrolet ((%call (name) `(case duplication-check
                                ((nil)           (,name movements-shrinked steady-state-tree nil nil))
                                (:post-only    (,name movements-shrinked steady-state-tree nil t))
                                (:forward-only (,name movements-shrinked steady-state-tree t nil))
                                (otherwise     (,name movements-shrinked steady-state-tree t t)))))
      (case verbose
        ((2 t)       (%call %verbose-tree))
        ((1 :modest) (%call %modest-tree))
        (otherwise   (%call %none-tree))))))
