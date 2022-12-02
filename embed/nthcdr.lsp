(lambda (n x) (block nil (while (and (>= (decf n) 0) x) (unless (consp x) (return nil)) (setq x (cdr x))) x))
