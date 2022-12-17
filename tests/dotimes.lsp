(test
	(let (n (sum 0))
		(dotimes (n 4) (setq sum (+ sum n)))
		sum)
	(+ 0 1 2 3))
