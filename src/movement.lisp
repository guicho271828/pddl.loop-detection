
(in-package :pddl.loop-detection)
(use-syntax :annot)

@export
(defun extract-movement (base tas)
  (mapcar
   (lambda (ta)
     (match ta
       ((timed-action
	 (action
	  (pddl-actual-action domain problem))
	 (end (timed-state state)))

	(mappend
	 (lambda (s)
	   (remove-if (curry #'eqname (object problem base))
		      (parameters s)))
	 (remove-if-not
	  (curry #'related-to base)
	  state)))))
   (filter-schedule tas :objects base)))


@export
(defun format-movement (result)
  (format t "
~{~{~4t~a~%~}~%~}
" (mapcar (lambda (elem) (mapcar #'name elem)) result)))














































;; ;; なぜ以下のようなこういうことをするか?
;; ;; --> baseの「場所」は (at ?base ?pos) だけには限られないから。
;; ;; move-arm-holding で運んでいる途中は、(hold ?arm ?base) から、
;; ;; base の場所は ?arm だと認識する必要がある。

;; (defun %appears (obj parametrized)
;;   (find obj (parameters parametrized)))

;; @export
;; (defun extract-base-positions (base timed-actions)
;;   (iter (with *domain* = nil)
;; 	(with *problem* = nil)
;; 	(for ta in (filter-schedule
;; 		    timed-actions
;; 		    :objects base))
;; 	(for info = 
;; 	     (match ta
;; 	       ((timed-action
;; 		 (action
;; 		  (and action (pddl-actual-action
;; 			       domain problem)))
;; 		 (start (timed-state (time time1))))
;; 		(setf *domain* domain *problem* problem)
;; 		(list time1
;; 		      action
;; 		      nil
;; 		      nil))))
;; 	(for pinfo previous info)
;; 	(collecting info)
	
;; 	;; (unless pinfo
;; 	;;   (next-iteration))
;; 	(when pinfo
;; 	  (setf (fourth pinfo) (first info))); set end-time information
;; 	(setf
;; 	 (third info)
;; 	 (%extract-base-position (object *problem* base) info pinfo))))

;; (defun %extract-base-position (base info pinfo)
;;   (flet ((base-related (preds)
;; 	   (remove-if-not (curry #'%appears base) preds)))
;;     (let* ((action (second info))
;; 	   (d (delete-list action))
;; 	   (a (add-list action))
;; 	   (p (positive-preconditions action)))
;;       @ignorable action d a p 
;;       (or (when-let ((candidate
;; 		      (remove-if-not ; アクションのbase以外のパラメータobjで、
;; 		       (lambda (obj) ; add-effectにある述語のどれかの引数に現れるもの
;; 			 (some (curry #'%appears obj)
;; 			       (base-related a)))
;; 		       (remove-if (curry #'eqname base)
;; 				  (parameters action)))))
;; 	    ;; この時点で、少なくとも場所に変更rがある可能性があることがわかった。
;; 	    (if (and pinfo (third pinfo))
;; 		;; 一つ前の場所があることが前提(init アクションを除外)。
;; 		;; 場所が更新されるなら、古い方の場所は delete effect
;; 		;; に入っていないと行けない。
;; 		;; 
;; 		;; 場所の数は全体として変わってもいい。
;; 		;; (at b1 x1 y1) > (lifting b1 hoist) では (x1,y1) から (hoist)へ。
;; 		;; 
;; 		;; 今回のdelete effectのうちbaseに関連する述語(1)について、
;; 		;; そのbase以外の引数(2)が 
;; 		;; 前回の場所のリスト(3)の中に含まれている場合、
;; 		;; (3)は正しかったという事になる。
;; 		;;
;; 		;; 含まれない場合、・・・も正しいのか。変更がないだけ。
;; 		;; 
;; 		;; まあだから、前回と今回とでマージしないといけない。
		
;; 		(append
;; 		 candidate
;; 		 (remove-if
;; 		  (lambda (obj)
;; 		    (some (curry #'%appears obj) (base-related d)))
;; 		  (third pinfo)))

;; 		;; 前回が無い場合(init アクション)、candidateをそのまま使う。
;; 		candidate))
;; 	  (third pinfo))))) ;;なにも該当するものがなければ、一つ前の場所のまま





;; @export
;; (defun print-positions (positions &optional (s *standard-output*))
;;   (mapc (lambda (list)
;; 	  (match list
;; 	    ((list start action places _)
;; 	     (format s "
;; at t=~a, ~10taction ~a is run.
;; ~10tafter this action the place is:

;; ~{~4t~a~^~%~}
;; "
;; 		     start
;; 		     action
;; 		     (mapcar #'name places)))))
;; 	positions)
;;   nil)