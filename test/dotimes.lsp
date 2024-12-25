(test
	(let ((sum 0))
		(dotimes (n 4 (* sum 2))
      (setq sum (+ sum n))))
	(* (+ 0 1 2 3) 2))
