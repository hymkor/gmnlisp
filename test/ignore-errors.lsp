(let ((x 1))
  (assert-eq (ignore-errors x) 1)
  (assert-eq (ignore-errors x (+ 1 1)) 2)
  (assert-eq (ignore-errors y (+ 1 1)) nil))
