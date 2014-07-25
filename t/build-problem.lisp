(in-package :pddl.loop-detection-test)
(in-suite :pddl.loop-detection)




(test (build-problem)
  (finishes
    (iter (repeat 15)
          (for (values plan ss handler)
               initially (best-first-mfp *movements* :verbose nil)
               then (funcall handler 0))
          (unless (and handler ss)
            (finish))
          (for problem = (loop-problem makep ss
                              *schedule* *movements*
                              (object makep :p2)))
          (pass)
          (print problem)
          (terpri)
          (print-pddl-object problem *standard-output*))))


;;;; write-problem
;; (let ((tmpdir (merge-pathnames (string-downcase (gensym "cell-assebly")) #p"/tmp/")))

;;     (for-all ((problem1 (lambda () (random-elt steady-state-problems))))
;;       ;; regression test : domain is always bound
;;       (is-true (domain problem1))
;;       ;; regression test : BASE0 is always included
;;       (is-true (some (lambda (obj)
;;                        (search "BASE0" (symbol-name (name obj))))
;;                      (objects/const problem1)))
      
;;       (let ((path (write-problem problem1 tmpdir)))
;;         (let ((newprob (symbol-value
;;                         (let ((*package* (find-package :pddl.instances)))
;;                           (handler-bind ((found-in-dictionary
;;                                           #'muffle-warning))
;;                             (parse-file path))))))
;;           (is (string= (symbol-name (name newprob))
;;                        (symbol-name (name problem1))))
;;           (is (set-equal (objects newprob)
;;                          (objects problem1)
;;                          :test #'string=
;;                          :key (lambda (obj)
;;                                 (symbol-name (name obj))))))))
;;     (inferior-shell:run
;;      `(rm -rfv ,tmpdir)))
