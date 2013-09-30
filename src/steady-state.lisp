(in-package :pddl.loop-detection)
(use-syntax :annot)

@export
(defun make-eol (steady-state length)
  "make the end-of-loop state"
  `(,@(cdr steady-state) ,length))

@export
(defun same-end-loop-p (max a b)
  (equalp (make-eol a max)
          (make-eol b max)))

(defun %shrink-bucket (bucket max)
  (sort
   (remove-duplicates
    bucket
    :test (curry #'same-end-loop-p max))
   #'<
   :key (lambda (lst)
          (reduce (lambda (prev now) (+ now (* max prev))) (reverse lst)))))

@export
@doc "Shrinks the number of steady states. Built-in function of exploit-steady-states"
(defun shrink-steady-states (steady-states)
  (let (acc)
    (maphash (lambda (key value)
               @ignore key
               (appendf acc (remove-duplicates value :test #'equalp)))
             (categorize steady-states :key #'length))
    acc))

@export @doc "returns a list of steady states. Each steady state is a
list of mutex position index.  0 indicates carry-in, where a base
is waiting to be carried into the factory and where no mutex
exists. Any of the resulting steady-states must have one base placed
at carry-in."
(defun exploit-steady-states (movements-shrinked)
  (unwind-protect
       (handler-return ((storage-condition
                         (lambda (c)
                           @ignore c
                           nil)))
         (shrink-steady-states
          (%exploit-steady-states movements-shrinked)))
    (sb-ext:gc :full t)))

@export
(defun %exploit-steady-states (movements-shrinked)
  (let ((movements-without-carry-in (cdr movements-shrinked)))
     (let ((max (length movements-without-carry-in)))
       (mapcon
        (lambda (rest)
          (%exploit-rec rest
                        nil
                        '(0)
                        (1+ (- max (length rest)))))
        movements-without-carry-in))))

@export
(defun mutices-no-conflict-p (this used-mutices)
  (dolist (bucket used-mutices)
    (dolist (e1 this)
      (dolist (e2 bucket)
        (when (eqstate e1 e2)
          (return-from mutices-no-conflict-p nil)))))
  t)

(defun %exploit-rec (movements-shrinked used-mutices base-positions i)
  ;(break+ used-mutices (car movements-shrinked))
  ;; (print (reverse
  ;;         (cons i base-positions)))
  (match movements-shrinked
    ((list this)
     (%exploit-leaf this used-mutices base-positions i))
    ((list* this rest)
     (if (mutices-no-conflict-p this used-mutices)
         (let ((next-mutices (cons this used-mutices))
               (next-base-positions (cons i base-positions))
               (len (length rest)))
           (cons (reverse next-base-positions)
                 (iter (for rest2 on rest)
                       (for next-i from (+ 1 i) to (+ 1 i len))
                       (when-let ((children
                                   (%exploit-rec
                                    rest2
                                    next-mutices
                                    next-base-positions
                                    next-i)))
                         ;; !!! USING NCONC !!!
                         ;; Using NCONC in order to reduce consing.
                         ;; This is justified because it is just accumulating the results
                         ;; in each branch. This is the same as pushing things to accumelator.
                         (nconcing children)))))
         (%exploit-rec rest
                       used-mutices
                       base-positions
                       (1+ i))))))

(defun %exploit-leaf (this used-mutices base-positions i)
  (if (mutices-no-conflict-p this used-mutices)
      (list (reverse (cons i base-positions)))
      nil))


