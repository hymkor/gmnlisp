(test
  (let (n (sum 0))
      (dolist (n '(1 3 5 7))
        (setq sum (+ sum n)))
      sum)
  (+ 1 3 5 7))
