(lambda-macro (vars &rest body)
  (let ((var (car vars))
        (values (elt vars 1))
        (result (if (> (length vars) 2) (elt vars 2) nil))
        (rest (gensym)))
    `(block nil
       (let ((,var nil) (,rest ,values))
         (while ,rest
           (setq ,var (car ,rest))
           (setq ,rest (cdr ,rest))
           ,@body) ,result))))
