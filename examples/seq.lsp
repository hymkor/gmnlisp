(defun seq (n)
  (format t "~s~%" n)
  (cond
    ((equal n 0)
      (return-from seq nil))
    (t
      (seq (- n 1)))
  )
)
(seq 10)
