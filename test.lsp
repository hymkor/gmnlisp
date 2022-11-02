(defmacro assert (func expect)
  (let ((result (gensym)))
    `(let ((,result ,func))
       (if (not (equal ,result ,expect))
         (progn
           (format t "Failed: ~s~%" (quote ,func))
           (format t "  expect: ~s but ~s~%" ,expect ,result)
           (quit))))))

(assert
  (let ((A (create-array '(3 2) 1)))
    (elt A 1 1))
  1)

