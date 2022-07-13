(let ((x 1)) 
  (defun f ()
    (list x)))

(let ((x 2))
  (f))
