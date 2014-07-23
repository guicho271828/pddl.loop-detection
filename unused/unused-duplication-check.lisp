

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
