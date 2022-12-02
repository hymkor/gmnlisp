(lambda-macro (vars &rest body)
  (let ((var (car vars)) (values (elt vars 1)) (rest (gensym)))
    `(block nil
       (let ((,var nil) (,rest ,values))
         (while ,rest
           (setq ,var (car ,rest))
           (setq ,rest (cdr ,rest))
           ,@body)))))
