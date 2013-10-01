#|
This file is a part of pddl.loop-detection project.
|#

(in-package :cl-user)
(defpackage pddl.loop-detection-test
  (:use :cl
	:guicho-utilities
	:guicho-a*
	:guicho-red-black-tree
	:iterate
	:alexandria
	:optima
	:repl-utilities
        :pddl
	:pddl.scheduler
        :pddl.loop-detection
	:pddl.plan-optimizer
        :pddl.instances
        :fiveam)
  (:shadow :place :maximize :minimize)
  (:shadowing-import-from :guicho-a* :cost)
  (:shadowing-import-from :fiveam :fail))
(in-package :pddl.loop-detection-test)

(def-suite :pddl.loop-detection :in :pddl)
(in-suite :pddl.loop-detection)

(defparameter schedule
  (reschedule
   cell-assembly-model2b-2-7
   :minimum-slack))

;; 輸送型のアクションを検出できれば - 場所がわかるかも。
;; 
;; どうやら、ベースごとのアクション列は同じみたいだ。(n=2の場合。)
;; 理論的に検証できるか?
;; 
;; 今は lama-2011-opt だが、sat ではどうか?

;; 場所の列だけでのか?

;; 別にアクション列は同じじゃなかった。
;; (test same-actions-per-base
;;   (is (every (lambda (ta0 ta1)
;; 	       (eq (name (timed-action-action ta0))
;; 		   (name (timed-action-action ta1))))
;; 	     (filter-schedule schedule :objects '(b-0))
;; 	     (filter-schedule schedule :objects '(b-1)))))

(defvar movements)
(defvar movements-indices)
(defvar movements-shrinked)
(defvar movements-indices-shrinked)

(test extract-movements
  (finishes
    (multiple-value-setq
	(movements movements-indices)
      (%extract-movements 'b-0 schedule cell-assembly)))
  (dolist (m (butlast (cdr movements)))
    (is-true m))
  (finishes
    (multiple-value-setq
	(movements-shrinked movements-indices-shrinked)
      (shrink-movements movements movements-indices)))
  (dolist (m (butlast (cdr movements-shrinked)))
    (is-true m)))

(defvar steady-states)

(test (steady-states :depends-on extract-movements)
  (finishes
    (setf steady-states 
	  (exploit-steady-states movements-shrinked)))
  (for-all ((ss (lambda () (random-elt steady-states))))
    (when (>= (length ss) 2)
      (map-combinations
       (lambda (list)
         (is (null (intersection
                    (first list) (second list)
                    :test #'eqstate))))
       (mapcar (rcurry #'nth movements-shrinked) ss)
       :length 2))
    (is (= 0 (first ss)))))

(test (search-loop-path :depends-on steady-states)
  (time (search-loop-path
	 movements-shrinked
	 (lastcar steady-states))))

(defvar loopable-steady-states)
(test (loopable-steady-states :depends-on steady-states)
  (format t "~%testing loopable-steady-states. It takes time so please wait...~2%")
  (finishes
    (time (setf loopable-steady-states
		(exploit-loopable-steady-states
                 movements-shrinked
                 steady-states :verbose :modest)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; test tree

(test tree-iterator
  (let ((it (tree-iterator '(0 1 2))))
    (is (equal '(0) (funcall it)))
    (is (equal '(0 1) (funcall it)))
    (is (equal '(0 2) (funcall it))))

  (let ((it (tree-iterator '(0
                             (1 2 3)
                             4
                             (2
                              (3 4 6)
                              (7 8 9))
                             5))))
    (is (equal '(0) (funcall it)))
    (is (equal '(0 1) (funcall it)))
    (is (equal '(0 1 2) (funcall it)))
    (is (equal '(0 1 3) (funcall it)))
    (is (equal '(0 4) (funcall it)))
    (is (equal '(0 2) (funcall it)))
    (is (equal '(0 2 3) (funcall it)))
    (is (equal '(0 2 3 4) (funcall it)))
    (is (equal '(0 2 3 6) (funcall it)))
    (is (equal '(0 2 7) (funcall it)))
    (is (equal '(0 2 7 8) (funcall it)))
    (is (equal '(0 2 7 9) (funcall it)))
    (is (equal '(0 5) (funcall it))))

  (let ((it (tree-iterator '(0
                             (1 2 3)
                             4
                             (2
                              (3 4 6 (7 -1 9) (10 11) 12 13)
                              (7 8 9))
                             5))))
    (is (equal '(0) (funcall it)))
    (is (equal '(0 1) (funcall it)))
    (is (equal '(0 1 2) (funcall it)))
    (is (equal '(0 1 3) (funcall it)))
    (is (equal '(0 4) (funcall it)))
    (is (equal '(0 2) (funcall it)))
    (is (equal '(0 2 3) (funcall it)))
    (multiple-value-bind (value stack) (funcall it)
      (is (equal '(0 2 3 4) value))
      (is (equalp '(3 4 6 (7 -1 9) (10 11) 12 13) stack))
      (is (equalp '(2
                    (3 4 6 (7 -1 9) (10 11) 12 13)
                    (7 8 9))
                  (funcall it :wind-stack stack))))

    (multiple-value-bind (value stack) (funcall it)
      (is (equal '(0 2 7) value))
      (is (equalp '(7 8 9) stack)))
      
    (is (equal '(0 2 7 8) (funcall it)))
    (is (equal '(0 2 7 9) (funcall it)))
    (is (equal '(0 5) (funcall it)))
    (signals tree-exhausted (funcall it))))

(test (lazy-tree-iterator :depends-on tree-iterator)
  (let ((it (tree-iterator (llist 0 1 2) :lazy t)))
    (is (equal '(0) (funcall it)))
    (is (equal '(0 1) (funcall it)))
    (is (equal '(0 2) (funcall it)))
    (signals tree-exhausted (funcall it)))

  (let ((it (tree-iterator (ltree (0
                                   (1 2 3)
                                   4
                                   (2
                                    (3 4 6)
                                    (7 8 9))
                                   5))
                           :lazy t)))
    (is (equal '(0) (funcall it)))
    (is (equal '(0 1) (funcall it)))
    (is (equal '(0 1 2) (funcall it)))
    (is (equal '(0 1 3) (funcall it)))
    (is (equal '(0 4) (funcall it)))
    (is (equal '(0 2) (funcall it)))
    (is (equal '(0 2 3) (funcall it)))
    (is (equal '(0 2 3 4) (funcall it)))
    (is (equal '(0 2 3 6) (funcall it)))
    (is (equal '(0 2 7) (funcall it)))
    (is (equal '(0 2 7 8) (funcall it)))
    (is (equal '(0 2 7 9) (funcall it)))
    (is (equal '(0 5) (funcall it)))
    (signals tree-exhausted (funcall it)))

  (let ((it (tree-iterator (ltree (0
                                   (1 2 3)
                                   4
                                   (2
                                    (3 4 6 (7 -1 9) (10 11) 12 13)
                                    (7 8 9))
                                   5))
                           :lazy t)))
    (is (equal '(0) (funcall it)))
    (is (equal '(0 1) (funcall it)))
    (is (equal '(0 1 2) (funcall it)))
    (is (equal '(0 1 3) (funcall it)))
    (is (equal '(0 4) (funcall it)))
    (is (equal '(0 2) (funcall it)))
    (is (equal '(0 2 3) (funcall it)))
    (multiple-value-bind (value stack) (funcall it)
      (is (equal '(0 2 3 4) value))
      (is-true (ematch stack
                 ((list* 3 4 _) t)))
      (is-true (ematch (funcall it :wind-stack stack)
                 ((list* 2 (list* 3 4 _) _) t))))

    (multiple-value-bind (value stack) (funcall it)
      (is (equal '(0 2 7) value))
      (is-true (ematch stack
                 ((cons 7 _) t))))
      
    (is (equal '(0 2 7 8) (funcall it)))
    (is (equal '(0 2 7 9) (funcall it)))
    (is (equal '(0 5) (funcall it)))
    (signals tree-exhausted (funcall it))))

(defvar steady-state-tree)
(test (steady-state-tree :depends-on extract-movements)
  (finishes
    (setf steady-state-tree
	  (exploit-steady-state-tree movements-shrinked))))

(defvar loopable-steady-state-tree)
(test (loopable-steady-state-tree :depends-on loopable-steady-states)
  (finishes
    (setf loopable-steady-state-tree
          (exploit-loopable-steady-state-tree
           movements-shrinked steady-state-tree :verbose :modest))))

(defparameter prob
  cell-assembly-model2b-2)
(defparameter base-type
  (type (object prob 'b-0)))

(defvar steady-state-problems)

(test (build-problem :depends-on loopable-steady-states)
  (finishes
    (time
     (setf steady-state-problems
	   (iter (for loop-plan in loopable-steady-states)
		 (collect
		     (build-steady-state-problem
		      prob loop-plan schedule
		      movements-shrinked movements-indices-shrinked base-type)))))))

(test (build-problem-1 :depends-on build-problem)
  
  ;; regression test : the conses are always fresh
  (for-all ((problem1 (curry #'random-elt steady-state-problems))
            (problem2 (curry #'random-elt steady-state-problems)
                      (not (eq problem1 problem2))))
    (is-false (equal (goal problem1) (goal problem2)))))

(test the-test
  (is-true 5)
  (is (= 5 5))
  (is (identity t))
  (is-false nil))


;; state が 自分のmutexになっているようなowner命題を探す。
;; この計算を行うためには owner,mutex,indicesが必要
(defun %find-owner (owner mutex indices state init)
  (find-if
   (lambda (owner-state?)
     (and (predicate-more-specific-p owner-state? owner)
          (pddl.loop-detection::%matches-to-mutex-p
           mutex indices owner-state? state)))
   init))

;; state のmutex命題を探す。
;; この計算を行うためには、owner,mutex,indicesが必要
;; owner はいらない。stateを代わりに使えるから
(defun %find-mutex (mutex indices state init)
  (find-if
   (lambda (mutex-state?)
     (pddl.loop-detection::%matches-to-mutex-p
      mutex indices state mutex-state?))
   init))

(test (build-problem-2 :depends-on build-problem)
  ;; regression test : ensures mutex predicates
  (for-all ((problem (lambda () (random-elt steady-state-problems))))
    (let* ((init (init problem))
           (domain (domain problem))
           (descriptors (mutex-predicates domain)))
      (dolist (desc descriptors)
        (match desc
          ((list owner release indices :release _)
           (dolist (state init)
             (cond
               ((predicate-more-specific-p state owner)
                (let ((found (%find-mutex release indices state init)))
                  (is-false
                   found
                   "
release predicate
 ~a was found though its owner
 ~a is already declared"
                   found state)))
               ((predicate-more-specific-p state release)
                (let ((found (%find-owner owner release indices state init)))
                  (is-false
                   found
                   "
owner predicate
 ~a was found though its release
 ~a is already declared"
                   found state))))))
          ((list owner mutex indices :mutex _)
           (dolist (state init)
             (cond
               ((predicate-more-specific-p state owner)
                (is-true
                 (%find-mutex mutex indices state init)
                 "
no instance of mutex predicate
 ~a was found though an owner
 ~a is declared"
                 mutex state))
               ((predicate-more-specific-p state mutex)
                (is-true
                 (%find-owner owner mutex indices state init)
                 "
no instance of owner predicate
 ~a was found though a mutex
 ~a is declared"
                 owner state))))))))))

(test (write-problem :depends-on build-problem)
  
  (let ((tmpdir (merge-pathnames (string-downcase (gensym "cell-assebly")) #p"/tmp/")))

    (for-all ((problem1 (lambda () (random-elt steady-state-problems))))
      ;; regression test : domain is always bound
      (is-true (domain problem1))
      ;; regression test : BASE0 is always included
      (is-true (some (lambda (obj)
                       (search "BASE0" (symbol-name (name obj))))
                     (objects/const problem1)))
      
      (let ((path (write-problem problem1 tmpdir)))
        (let ((newprob (symbol-value
                        (let ((*package* (find-package :pddl.instances)))
                          (handler-bind ((found-in-dictionary
                                          #'muffle-warning))
                            (parse-file path))))))
          (is (string= (symbol-name (name newprob))
                       (symbol-name (name problem1))))
          (is (set-equal (objects newprob)
                         (objects problem1)
                         :test #'string=
                         :key (lambda (obj)
                                (symbol-name (name obj))))))))
    (inferior-shell:run
     `(rm -rfv ,tmpdir))))

(test integrated
  (terpri)
  (finishes
    (exploit-loop-problems cell-assembly-model2b-2-7 'b-0)))
(test integrated-tree
  (terpri)
  (finishes
    (exploit-loop-problems-tree cell-assembly-model2b-2-7 'b-0)))
