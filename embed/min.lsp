(lambda (x :rest r)
  (assure <number> x)
  (while r
    (if (> x (assure <number> (car r)))
      (setq x (car r)))
    (setq r (cdr r)))
  x)

