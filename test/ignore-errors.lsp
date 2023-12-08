(let ((x 1))
  (test (ignore-errors x) 1)
  (test (ignore-errors x (+ 1 1)) 2)
  (test (ignore-errors y (+ 1 1)) nil))
