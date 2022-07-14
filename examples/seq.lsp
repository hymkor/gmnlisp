(defun seq (n)
  (print n)
  (cond
    ((equal n 0)
      (return-from seq nil))
    (T
      (seq (- n 1)))
  )
)
(seq 10)
