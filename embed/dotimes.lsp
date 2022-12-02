(lambda-macro (vars &rest commands) (let ((var (car vars)) (count (elt vars 1)) (end (gensym))) `(let ((,var 0) (,end ,count)) (while (< ,var ,end) (progn ,@commands) (setq ,var (+ 1 ,var))))))
