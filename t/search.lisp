(in-package :pddl.loop-detection-test)
(in-suite :pddl.loop-detection)

(defun run-sleep (n)
  (sb-ext:run-program
   "sleep" (list (princ-to-string n))
   :search t :wait nil :output t :error :output))

(test async-shell
  ;; pass!
  (finishes
    (let ((process (run-sleep 1)))
      (sb-ext:process-wait process t)))
  ;; pass!
  (let ((process (run-sleep 100)))
    (ignore-errors
      (unwind-protect
           (error "error!")
        (sb-ext:process-kill process 9))) ; SIGKILL
    (sb-ext:process-wait process)
    (is-false (sb-ext:process-alive-p process)))
  ;; pass!
  (is (eq :finished
          (let ((process (run-sleep 1)))
            (with-timeout (100) ; timeout does not occur
              (print :waiting)
              (sb-ext:process-wait process t)
              (print :finished)))))
  ;; ;; Control stack guard page temporarily disabled: proceed with caution
  ;; (signals timeout-error
  ;;   (let ((process (run-sleep 100)))
  ;;     (with-timeout (5)
  ;;       (print :waiting)
  ;;       (sb-ext:process-wait process t)
  ;;       (print :finished))))
  (let* ((exited nil)
         (finished nil)
         (s *standard-output*)
         (th (make-thread 
              (lambda ()
                (let ((process (run-sleep 10)))
                  (unwind-protect
                       (progn (sb-ext:process-wait process t)
                              (setf finished t))
                    (setf exited t)
                    (sb-ext:process-kill process 9)))))))
    (sleep 1)
    (destroy-thread th)
    (sleep 1)
    (is-true exited)
    (is-false finished)))

;; run them automatically
(test exploit-loop-problems
  (finishes
    (let ((*domain* make) (*problem* makep))
      (exploit-loop-problems
       (pddl-plan :actions (parse-plan +makeplan+))
       (object *problem* :p1)
       (constant-results 0)
       :verbose t))))

(test exploit-loop-problems-with-timeout
  (let (start end result)
    (setf start (get-universal-time))
    (let ((*domain* make) (*problem* makep))
      (exploit-loop-problems
       (pddl-plan :actions (parse-plan +makeplan+))
       (object *problem* :p1)
       (constant-results 0)
       :verbose t
       :timeout 1))
    (setf end (get-universal-time))
    (is (< (- end start) 2))))



