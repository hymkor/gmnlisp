(lambda-macro (vars &rest commands)
  (let ((var (car vars))
        (count (elt vars 1))
        (result (if (> (length vars) 2) (elt vars 2) nil))
        (end (gensym)))
    `(let ((,var 0) (,end ,count))
       (while (< ,var ,end)
         (progn ,@commands)
         (setq ,var (+ 1 ,var))) ,result)))
