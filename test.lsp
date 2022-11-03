(defmacro assert (func expect)
  (let ((result (gensym)))
    `(let ((,result ,func))
       (if (not (equal ,result ,expect))
         (progn
           (format t "Failed: ~s~%" (quote ,func))
           (format t "  expect: ~s but ~s~%" ,expect ,result)
           (abort))))))

(let ((A (create-array '(3 3) 0)))
  (assert (aref A 1 1) 0)
  (set-aref 2 A 1 1)
  (assert (aref A 1 1) 2)
  ;(format t "~s~%" (aref A 1))
  )

(let ((A (create-array '(3 2) 1)))
  (assert (arrayp A) t)
  (set-aref 2 A 1 1)
  (set-aref (create-array '(2) 4) A 0)
  (setf (elt A 2 1) "SETF")
  (assert (elt A 0 0) 4)
  (assert (elt A 0 1) 4)
  (assert (elt A 0) (create-array '(2) 4))
  (assert (elt A 1 1) 2)
  (assert (elt A 1 0) 1)
  (assert (elt A 2 1) "SETF")
  )

(let ((A #2a( (1 2 3) (4 5 6) )))
  (assert (aref A 0 0) 1)
  (assert (aref A 0 1) 2)
  (assert (aref A 0 2) 3)
  (assert (aref A 1 0) 4)
  (assert (aref A 1 1) 5)
  (assert (aref A 1 2) 6)
  )
