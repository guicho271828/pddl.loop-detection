
(in-package :pddl.loop-detection)
(use-syntax :annot)

;; なぜ以下のようなこういうことをするか?
;; --> baseの「場所」は (at ?base ?pos) だけには限られないから。
;; move-arm-holding で運んでいる途中は、(hold ?arm ?base) から、
;; base の場所は ?arm だと認識する必要がある。

(defun %appears (obj parametrized)
  (find obj (parameters parametrized)))

@export
(defun extract-base-positions (base timed-actions)
  (iter (with *domain* = nil)
	(with *problem* = nil)
	(for ta in (filter-schedule
		    timed-actions
		    :objects base))
	(for info = 
	     (match ta
	       ((timed-action
		 (action
		  (and action (pddl-actual-action
			       domain problem)))
		 (start (timed-state (time time1))))
		(setf *domain* domain *problem* problem)
		(list time1
		      action
		      nil
		      nil))))
	(for pinfo previous info)
	(collecting info)
	
	;; (unless pinfo
	;;   (next-iteration))
	(when pinfo
	  (setf (fourth pinfo) (first info))); set end-time information
	(setf
	 (third info)
	 (%extract-base-position (object *problem* base) info pinfo))))

(defun %extract-base-position (base info pinfo)
  (flet ((base-related (preds)
	   (remove-if-not (curry #'%appears base) preds))
	 (not-base-related (preds)
	   (remove-if (curry #'%appears base) preds)))
    (let* ((action (second info))
	   (d (delete-list action))
	   (a (add-list action))
	   (p (positive-preconditions action)))
      @ignorable action d a p 
      (remove-if-not
       (lambda (obj)
	 (labels ((in (list) (some (curry #'%appears obj) list))
		  (out (list) (not (in list))))
	   ;; add rules here!	     
	   ;; とりあえず場所の指示子は一つとして考える
	   (and (in (base-related a))
		;;(base-related pd)
		(if (and pinfo (third pinfo))
		    (some
		     (lambda (pred)
		       (subsetp
			(remove-if
			 (curry #'eqname base)
			 (parameters pred))
			(third pinfo)))
		     (base-related d))
		    t))))
       (remove-if (curry #'eqname base) (parameters action))))))


@export
(defun print-positions (positions &optional (s *standard-output*))
  (mapc (lambda (list)
	  (match list
	    ((list start action places _)
	     (format s "
at t=~a, ~10taction ~a is run.
~10tafter this action the place is:

~{~4t~a~^~%~}
"
		     start
		     action
		     (mapcar #'name places)))))
	positions)
  nil)