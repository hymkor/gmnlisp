(lambda (x :rest r)
  (while r
    (if (> x (car r))
      (setq x (car r)))
    (setq r (cdr r)))
  x)

