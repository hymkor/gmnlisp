(lambda (a b)
  (assure <integer> a)
  (assure <integer> b)
  (cond
    ((= a 0) 0)
    ((= b 0) 0)
    (t (* (div a (gcd a b)) b))))
