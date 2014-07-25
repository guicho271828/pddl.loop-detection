(in-package :pddl.loop-detection-test)
(in-suite :pddl.loop-detection)

(defvar *movements*)

(test (movements :depends-on basis)
  (finishes
    (setf *movements*
        (extract-movements :p2 (sort-schedule *schedule*) makep)))
  (match *movements*
    ((list (list 3 (pddl-atomic-state
                    :name 'making
                    :parameters
                    (list (pddl-object :name 'm1)
                          (pddl-object :name 'p2))))
           (list 6)
           (list 9 (pddl-atomic-state
                    :name 'making
                    :parameters
                    (list (pddl-object :name 'm2)
                          (pddl-object :name 'p2))))
           (list 11))
     (pass "the schedule works as expected"))))

(test (steady-state :depends-on movements)
  (is (equalp
       '(0 (1 (2 (3)) (3)) (2 (3)) (3))
       (steady-state *movements* nil))))


(test (mfp :depends-on movements)
  (is (equalp '(((0) (1) (2) (3) (4) (5)))
              (mutex-focused-planning *movements* '(0)))))

(test (filtering :depends-on movements)
  (let ((searcher (mfp-with-filtering *movements* :verbose t))
        (tree (steady-state *movements*))
        (current (make-array (length *movements*)
                             :element-type 'fixnum
                             :adjustable t
                             :fill-pointer 0)))
    (setf tree (fmapcar #'identity tree))
    (is (= 0 (car tree)))
    (vector-push (car tree) current)
    (is (equalp '(((0) (1) (2) (3) (4) (5)))
                (funcall searcher (coerce current 'list))))
    (iter (for lbranch in (cdr tree))
          (for branch = (fmapcar #'identity lbranch))
          (is (numberp (car branch)))
          (is (listp (cdr branch)))
          (vector-push (car branch) current)
          (finishes
            (print
             (funcall searcher (coerce current 'list))))
          (vector-pop current))))

(test (bfs-mfp :depends-on movements)
  (iter (repeat 5)
        (for (values plan ss handler)
             initially (best-first-mfp *movements* :verbose nil)
             then (funcall handler 0))
        (finishes
          (print plan)))
  (iter (repeat 5)
        (for (values plan ss handler)
             initially (best-first-mfp *movements* :verbose nil)
             then (funcall handler (random 5)))
        (finishes
          (print plan)))
  ;; what happens when the number exceeds 8 ?
  (signals warning
    (iter (repeat 15)
          (for (values plan ss handler)
               initially (best-first-mfp *movements* :verbose nil)
               then (funcall handler (random 5)))
          (print plan))))


;; run them automatically
(test exploit-loop-problems
  (finishes
    (let ((*domain* make) (*problem* makep))
      (exploit-loop-problems
       (pddl-plan :actions (parse-plan +makeplan+))
       (object *problem* :p1)
       (constantly 0)
       :verbose t))))


