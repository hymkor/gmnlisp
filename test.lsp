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
  (assert (array-dimensions A) '(3 2))
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
  (assert (array-dimensions A) '(2 3))
  (assert (aref A 0 0) 1)
  (assert (aref A 0 1) 2)
  (assert (aref A 0 2) 3)
  (assert (aref A 1 0) 4)
  (assert (aref A 1 1) 5)
  (assert (aref A 1 2) 6)

  (setf (aref A 1 0) 44)
  (assert (aref A 1 0) 44)
  (setf (elt A 1 0) 77)
  (assert (elt A 1 0) 77)
  )

(defun string-format (&rest args)
  (let ((buffer (create-string-output-stream)))
    (apply #'format buffer args)
    (get-output-stream-string buffer)))

(let ((ahaha "ahaha"))
  (assert (string-format "[~s]" ahaha) "[\"ahaha\"]")
  )

;;;; test for (tagbody) and (go) 
; forward test
(let ((step 0))
  (tagbody
    (setq step 1)
    ;(format t "step-1~%")
    (go skip)
    ;(format t "step-2~%")
    (setq step 2)
    skip
    )
  (assert step 1))

; backword test
(let ((step 0))
  (assert
    (block
      break1
      (tagbody
        loop-tag
        ;(format t "step=~a~%" step)
        (if (> step 4)
          (return-from break1 step))
        (incf step)
        (go loop-tag)
        )
      )
    5))

; psetq test
(let ((x 1) (y 2))
  (psetq x y
         y x)
  (assert x 2)
  (assert y 1))

; when
(assert (when t 1 2 3) 3)
(assert (when nil 1 2 3) nil)

; unless
(assert (unless t 1 2 3) nil)
(assert (unless nil 1 2 3) 3)

; prog1
(assert (prog1 1 2 3 4) 1)
(assert (prog2 1 2 3 4) 2)

;;;; for(1) ;;;
(assert
  (let (x y)
    (for ((x 0 (1+ x)) (y 0 (+ y 10)))
         ((= x 5) (+ x y))
         ))
  55)

;;; for(2) ;;;
(defun fibo2 (n)
  (let (a b)
    (for ((n n (- n 1))
          (a 0 b)
          (b 1 (+ a b)))
         ((<= n 0) a))))
(assert (fibo2 10) 55)
