(in-package :pddl.loop-detection)
(use-syntax :annot)

#|
 (0 (1 2 3 4)
    (2 3))
represents the following steady-states:

 (0)
 (0 1)
 (0 1 2)
 (0 1 3)
 (0 1 4)
 (0 2)
 (0 2 3)

the total consing above is 4+2+3=9 and 17 respectively.
the number of conses is different between each other by an order of magnitude.
for each leaf node, the first structure requires only one cons, while 
the second requires N conses where N is the length of a steady state.
|#

(defvar *min*)

@export @doc "returns a cons tree of steady states. Each steady state is
represented by a leaf or a branch of the tree. Each leaf or a branch node
is a mutex position index.  0 indicates carry-in, where a base
is waiting to be carried into the factory and where no mutex
exists. Any of the resulting steady-states must have one base placed
at carry-in (= position 0)."
(defun exploit-steady-state-tree (movements-shrinked)
  (declare (optimize (speed 2) (debug 1)))
  @type list movements-shrinked
  (let ((*min* (length movements-shrinked)))
    (%tree-rec movements-shrinked nil 0)))

(declaim (ftype (function (list list fixnum) (or fixnum null)) %tree-leaf))
(defun %tree-leaf (this used-mutices i)
  (declare (optimize (speed 3) (debug 0) (safety 0) (space 0)))
  (if (mutices-no-conflict-p this used-mutices)
      i
      nil))

(declaim (ftype (function (list list fixnum) (or fixnum list)) %tree-rec))
(defun %tree-rec (movements-shrinked used-mutices i)
  (declare (optimize (speed 3) (debug 0) (safety 0) (space 0)))
  (match movements-shrinked
    ((list this)
     (%tree-leaf this used-mutices i))
    ((list* this rest)
     @type cons rest
     (if (mutices-no-conflict-p this used-mutices)
         (let ((next-mutices (cons this used-mutices))
               (len (length rest)))
           (prog1
               (cons i
                     (remove-duplicates
                      (iter (for rest2 on rest)
                            (for next-i
                                 from (the fixnum (+ 1 i))
                                 to (the fixnum (+ 1 i len)))
                            @type fixnum next-i
                            (when-let ((children
                                        (%tree-rec
                                         rest2
                                         next-mutices
                                         next-i)))
                              ;;(print children)
                              (collecting children)))
                      :test #'equal))
             (when (< i *min*)
               (setf *min* i)
               (print i))))
         (%tree-rec rest
                    used-mutices
                    (1+ i))))))


(declaim (ftype (function ((or cons fixnum) (or cons fixnum)) boolean) tree-duplication-test))

@export
(defun tree-duplication-test (a b)
  (declare (optimize (speed 3) (debug 0) (safety 0) (space 0)))
  (cond
    ((and (typep a 'fixnum) (typep b 'fixnum))
     (locally (declare (type fixnum a b))
       (= a b)))
    ((and (typep a 'cons) (typep b 'cons))
     (locally (declare (type cons a b))
       (tree-equal
        a b :test
        (lambda (c d)
          (declare (type fixnum c d))
          (= c d)))))))