
(in-package :pddl.loop-detection-test)
(in-suite :pddl.loop-detection)

(defparameter prob
  cell-assembly-model2b-1)
(defparameter base-type
  (type (object prob 'b-0)))

;;;; these tests are obsoleted, but might be helpful to read later

(test (build-problem-1 :depends-on build-problem)
  ;; regression test : the conses are always fresh
  (for-all ((problem1 (curry #'random-elt steady-state-problems))
            (problem2 (curry #'random-elt steady-state-problems)
                      (not (eq problem1 problem2))))
    (is-false (equal (goal problem1) (goal problem2)))))

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
    (exploit-loop-problems cell-assembly-model2b-1-1 'b-0)))

