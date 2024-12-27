(test
  (let ((z (list 0 0 0 0))
        (x (list 1 2 3 4))
        (y (list 5 6 7 8)))
    (map-into z #'+ x y)
    z)
  (list 6 8 10 12))

(test
  (let ((x (list 'a 'b 'c 'd))
        (y (list 'w 'x 'y))
        (z (list 0 0 0 0)))
    (map-into z (function cons) x y)
    z)
  (list (cons 'a 'w)
        (cons 'b 'x)
        (cons 'c 'y)
        0))
