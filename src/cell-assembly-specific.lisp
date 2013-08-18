
(in-package :pddl.loop-detection)
(use-syntax :annot)

;; なぜ以下のようなこういうことをするか?
;; --> baseの「場所」は (at ?base ?pos) だけには限られないから。
;; move-arm-holding で運んでいる途中は、(hold ?arm ?base) から、
;; base の場所は ?arm だと認識する必要がある。

(defun %appears (obj actual-state)
  (find obj (parameters actual-state)))

@export
(defun extract-base-positions (base timed-actions)
  (iter (for ta in (filter-schedule
		    timed-actions
		    :objects base))
	
	(for info = 
	     (match ta
	       ((timed-action
		 (action
		  (and action (pddl-action problem parameters)))
		 (start (timed-state (time time1))))
		(list time1 action
		      (remove-if
		       (lambda (param)
			 (eqname param (object problem base)))
		       parameters)
		      nil))))
	(for pinfo previous info)
	(for places = (third info))
	(for pplaces previous places)
	(collecting info)
	(unless pinfo
	  (next-iteration))
	
	(setf (fourth pinfo) (first info))
	(setf (third pinfo)
	      (intersection
		(remove-if-not
		 (let ((action (second pinfo)))
		   (lambda (obj)
		     (let ((fn (curry #'%appears obj)))
		       ;; add rules here!
		       (and (not (some fn (delete-list action)))
			    (or (some fn (positive-preconditions action))
				(some fn (add-list action)))))))
		 pplaces)
		
		(remove-if-not
		 (let ((action (second info)))
		   (lambda (obj)
		     (let ((fn (curry #'%appears obj)))
		       ;; add rules here!
		       (and (some fn (positive-preconditions action))))))
		 places)))))




@export
(defun print-positions (positions &optional (s *standard-output*))
  (mapc (lambda (list)
	  (match list
	    ((list start action places end)
	     (format s "
at t=~a, action ~a is run. after this action the place is:

~{~4t~w~^~%~}
"
		     start
		     (symbol-name (name action))
		     places
		     end))))
	positions))