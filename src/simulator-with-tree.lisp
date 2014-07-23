(in-package :pddl.loop-detection)
(use-syntax :annot)

;;;; utility


;; a bucket is a list.
;; make-buckets returns an array of nil.
(defun make-buckets (n)
  (make-array n :initial-element nil))

(defun %forward-duplication-check (ss end bucket)
  (some
   (lambda (loop-path)
     (or (find ss loop-path :test #'equal)
         (find end loop-path :test #'equal)))
   bucket))


(declaim (inline funcall-if-functionp))
(defun funcall-if-functionp (fn &rest arguments)
  (when (functionp fn)
    (apply fn arguments)))

@export
(define-condition steady-state-condition (error)
  ((steady-state :accessor steady-state :initarg :steady-state)))

(export '(steady-state skip-this wind-stack))

;;;; main

(progn
  (define-local-function %print-open (ss)
    (when verbose
      (format t "~&Fetching ~a " ss)))
  (define-local-function %skip-and-restart ()
    (when verbose
      (format t " ... Skipping"))
    (go :start))
  (define-local-function %wind-and-restart ()
    (format t " ... Winding")
    (funcall it :wind-stack stack)
    (go :start))

  (define-local-function %check-duplicated-and-maybe-restart ()
    ;; check if `ss' is duplicated
    (when (%forward-duplication-check
           ss
           (make-eol ss m-num)
           (aref buckets bases))
      (when verbose
        (format t " ... Duplicated"))
      (go :start)))

  (define-local-function %fail-and-restart ()
    (format t " ... No loop")
    (funcall it :wind-stack stack)
    (go :start))

  (declaim (inline exploit-loopable-steady-state-lazy))
  @export
  (defun exploit-loopable-steady-state-lazy
      (movements steady-state-tree &key (verbose t))
    "Returns the list of solution path from start-of-loop (MS3) to
end-of-loop (MS3)."
    (let* ((m-num (length movements))
           (it (tree-iterator steady-state-tree :lazy t))
           (buckets (make-buckets m-num)))
        (label1 rec ()
            (handler-return
                ((tree-exhausted (lambda (c)
                                   (declare (ignore c))
                                   (values nil buckets))))
              (let (ss stack)
                (tagbody
                 :start
                   (more-labels () (%print-open %skip-and-restart %wind-and-restart)
                     (multiple-value-setq (ss stack) (funcall it))
                     (%print-open)
                     (restart-bind
                         ((skip-this #'%skip-and-restart)
                          (wind-stack #'%wind-and-restart)
                          (continue (lambda () (go :cont))))
                       (error 'steady-state-condition :steady-state ss)))
                 :cont
                   (let ((bases (1- (length ss))))
                     (more-labels () (%check-duplicated-and-maybe-restart
                                      %fail-and-restart)
                       ;; forward-duplication-check
                       (%check-duplicated-and-maybe-restart)
                       (when-let ((results (search-loop-path movements ss :verbose verbose)))
                         (setf (aref buckets bases) (nconc results (aref buckets bases)))
                         (return-from rec
                           (values (cons results #'rec) buckets it)))
                       (%fail-and-restart))))))
          #'rec))))
