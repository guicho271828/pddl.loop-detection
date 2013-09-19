(in-package :pddl.loop-detection)
(use-syntax :annot)

@export
(defclass state-node (searchable-node)
  ((current-state :initarg :current-state :reader current-state)
   (goal :initarg :goal :accessor goal)
   (movements :initarg :movements :reader movements)
   (complementary-edge-class :initform 'transition)))

(defvar *state-hash*)
(defmethod allocate-instance :around
    ((class (eql (find-class 'state-node)))
     &key current-state goal)
  (or (gethash current-state *state-hash*)
      (let ((result (call-next-method)))
        (setf (gethash current-state *state-hash*) result)
        result)))

(defmethod print-object ((n state-node) s)
  (print-unreadable-object (n s :type t)
    (with-slots (current-state) n
      (format s "~a :cost ~a" current-state (cost n)))))

@export
(defclass transition (searchable-edge)
  ((complementary-node-class :initform 'state-node)))

(defmethod generic-eq ((n1 state-node) (n2 state-node))
  (equalp (current-state n1)
          (current-state n2)))

(defmethod cost ((tr transition))
  1)
(defmethod heuristic-cost-between ((n1 state-node) (n2 state-node))
  ;; this value is always the same, so it is meaningless.
  ;; (iter (for pos1 in (current-state n1))
  ;;    (for pos2 in (current-state n2))
  ;;    (summing (abs (- pos2 pos1))))
  0
  )

(defmethod constraint-ordering-op ((n state-node))
  0) ;; it was found to have no effect if any.

(defun get-loop-from-last (last)
  (iter (for node first last then (parent node))
        (while node)
        (collect (current-state node) at beginning)))

@export
(defun search-loop-path (movements-shrinked steady-state
                         &key (verbose t) (limit MOST-POSITIVE-FIXNUM))
  (let* ((*state-hash* (make-hash-table :test #'equalp))
         (goal (make-instance
                'state-node
                :movements movements-shrinked
                :current-state (make-eol steady-state
                                         (length movements-shrinked))))
         (start (make-instance
                 'state-node
                 :movements movements-shrinked
                 :goal goal
                 :current-state steady-state)))
    (setf (goal goal) goal)
    (let (solutions (cost 0))
      (handler-bind
          ((path-not-found
            (lambda (c)
              (declare (ignorable c))
              (when verbose
                (format t "~&Completely searched the state space!~%Paths found:~%~w"
                        solutions))))
           (solution-found
            (lambda (c)
              (let* ((last (solution c))
                     (path (get-loop-from-last last)))
                (when verbose
                  (format
                   t "~%Cost : ~w~%Solution : ~w"
                   (cost last) path))
                (cond
                  ((null solutions)
                   (push path solutions)
                   (setf cost (cost last))
                   (continue))
                  ((= cost (cost last))
                   (push path solutions)
                   (when (< (length solutions) limit)
                     (continue))))))))
        (a*-search start goal :verbose verbose))
      solutions)))

(declaim (ftype (function (list list state-node fixnum) boolean) %make-state-node))
(defun %make-state-node (movements-shrinked current-state goal n)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (make-instance
   'state-node
   :goal goal
   :movements movements-shrinked
   :current-state (substitute (the fixnum (1+ n)) n current-state)))


(declaim (ftype (function (fixnum fixnum list list) boolean) movable)) 
(defun movable (n goal-n used movements)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((n2 (1+ n))
        (carry-out-index (length movements)))
    @type fixnum carry-out-index
    @type fixnum n2
    (and ; include n if
     (not (= n goal-n)) ; the goal is not achieved yet
     (< n carry-out-index) ; n is not already the carry-out.
     (if (= carry-out-index n2) ; check the resource conflict.
         t          ; if n+1 is the carry-out, it doesn't consume resources.
         (null      ; else, the resource constraint should hold
          (intersection
           (nth n2 movements)
           (set-difference used (nth n movements)
                           :test #'eqstate)
           :test #'eqstate))))))

(defmethod generate-nodes ((state state-node))
  (with-slots (movements current-state) state
    ;; movements: (mutex*)*
    ;; current: number*
    (let* ((goal (goal state))
           (goal-state (current-state goal))
           (used (mappend (rcurry #'nth movements) current-state)))
      ;; used: mutices currently in use
      (mapcar
       (curry #'%make-state-node movements current-state goal)
       (iter (for n in current-state)
             (for goal-n in goal-state)
             (when (movable n goal-n used movements)
               (collect n)))))))

#|

steady-states, or ss: list of positions of bases like

 (0 3 5 16 19) ; the index of mutex aquired by base 0, 1, 2, 3, 4 

loop-path: list of steady-states, like

 ((0 3 5 16 19)
  (0 3 5 17 19)
  (0 3 6 17 19) ...)

bucket: list of loop-pathes. In each loop-path,
  all steady-states have the same number of bases.

  (((0 3 5 16 19) ; loop path 1
    (0 3 5 17 19) ; loop path 1
    (0 3 6 17 19) ...)  ; loop path 1
   ((0 4 5 16 19) ; loop path 2
    (0 4 5 17 19) ; loop path 2
    (0 4 6 17 19) ...)  ; loop path 2
     ... )

buckets: array of buckets. Each index corresponds to
the number of bases.

|#

(defun %report-results (max fdup-count pdup-count true-count)
  (format t
          "~%~1,80,80,'-a~%~{~80@<~40@a | ~10:a~>~%~}~1,80,80,'-a"
          "-"
          (list "All steady-states" max
                "Duplicated loops forward-detected" fdup-count
                "Duplicated loops post-detected" pdup-count
                "Valid loops in total" (+ true-count
                                          fdup-count
                                          pdup-count)
                "Valid loops w/o duplicated ones" true-count)
          "-"))

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
(defun %post-duplication-check/bucket (bucket)
  (remove-duplicates bucket :test #'%loop-equal))
(defun %loop-equal (path1 path2)
  (or (find (first-elt path1) path2 :test #'equalp)
      (find (last-elt path1) path2 :test #'equalp)
      (find (first-elt path2) path1 :test #'equalp)
      (find (last-elt path2) path1 :test #'equalp)))

(defun %merge-buckets (buckets1 buckets2)
  (map 'vector
       (lambda (bucket1 bucket2)
         (union bucket1 bucket2 :test #'%loop-equal))
       buckets1 buckets2))

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
                        (funcall-if-functionp after-success)
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
       (lambda ()
         (with-lock-held (*print-lock*)
           (format *shared-output* " ...success!")))
       (lambda ()
         (with-lock-held (*print-lock*)
           (format *shared-output* " ...failed.")))
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
    (let ((i 0) (i-lock (make-lock "i")))
      (%loop-verbosity
       (lambda ()
         (with-lock-held (i-lock)
           (incf i)
           (when (zerop (mod i 60))
             (with-lock-held (*print-lock*)
               (terpri *shared-output*)))))
       (lambda ()
         (with-lock-held (*print-lock*)
           (write-char #\. *shared-output*)))
       (lambda ()
         (with-lock-held (*print-lock*)
           (write-char #\F *shared-output*)))
       (lambda ()
         (with-lock-held (*print-lock*)
           (write-char #\D *shared-output*)))
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
                                (nil           (,name movements-shrinked steady-states nil nil))
                                (:post-only    (,name movements-shrinked steady-states nil t))
                                (:forward-only (,name movements-shrinked steady-states t nil))
                                (otherwise     (,name movements-shrinked steady-states t t)))))
      (case verbose
        ((2 t)       (%call %verbose))
        ((1 :modest) (%call %modest))
        (otherwise   (%call %none))))))
