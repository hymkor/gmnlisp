(lambda-macro (newvalue key m)
  (let ((L (gensym)) (K (gensym)) (tmp (gensym)))
    `(let* ((,L ,m) (,K ,key) (,tmp nil))
       (while ,L
         (if (and (setq ,tmp (car ,L))
                  (consp ,tmp)
                  (equal ,K (car ,tmp)))
           (set-car ,newvalue ,L))
         (setq ,L (cdr ,L))))))
