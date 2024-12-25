(test
  (let (n (sum 0))
      (dolist (n '(1 3 5 7) (* sum 9))
        (setq sum (+ sum n))))
  (* (+ 1 3 5 7) 9))
