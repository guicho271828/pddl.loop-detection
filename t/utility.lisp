(in-package :pddl.loop-detection-test)
(in-suite :pddl.loop-detection)

(defmacro with-effect ((name) effects &body body)
  `(let ((,name (or (find ',name ,effects :key #'name)
                    (warn "the effect named ~a is missing!"
                          ',name))))
     ,@body))
(defmacro with-effects (names effects &body body)
  (if (rest names)
      `(with-effect (,(first names)) ,effects
         (with-effects (,@(rest names)) ,effects
           ,@body))
      `(with-effect (,(first names)) ,effects
         ,@body)))
(defmacro with-add-effects (names action &body body)
  `(with-effects (,@names) (add-list ,action) ,@body))
(defmacro with-delete-effects (names action &body body)
  `(with-effects (,@names) (delete-list ,action) ,@body))
