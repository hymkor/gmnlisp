(lambda-macro (newvalue name avlue)
  (let ((name (elt expr 1))
        (value (elt expr 2)))
    `(progn
       (setq ,name ,value)
       (setf ,name ,newvalue))))
