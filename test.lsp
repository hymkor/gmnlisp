(defmacro assert (func expect)
  (let ((result (gensym)))
    `(let ((,result ,func))
       (if (not (equal ,result ,expect))
         (progn
           (format t "Failed: ~s~%" (quote ,func))
           (format t "  expect: ~s but ~s~%" ,expect ,result)
           (abort))))))

;;; test for create-array ;;;
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

;;; test for the constructor for array ;;;
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

;;; test for (tagbody) and (go)
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

;;; test for (psetq) ;;;
(let ((x 1) (y 2))
  (psetq x y
         y x)
  (assert x 2)
  (assert y 1))

;;; test for (when) ;;;
(assert (when t 1 2 3) 3)
(assert (when nil 1 2 3) nil)

;;; test for (unless) ;;;
(assert (unless t 1 2 3) nil)
(assert (unless nil 1 2 3) 3)

;;; test for (prog1) ;;;
(assert (prog1 1 2 3 4) 1)
(assert (prog2 1 2 3 4) 2)

;;;; test for (for) ;;;
(assert
  (let (x y)
    (for ((x 0 (1+ x)) (y 0 (+ y 10)))
         ((= x 5) (+ x y))
         ))
  55)

(defun fibo2 (n)
  (let (a b)
    (for ((n n (- n 1))
          (a 0 b)
          (b 1 (+ a b)))
         ((<= n 0) a))))
(assert (fibo2 10) 55)

;;; test for (format-float) ;;;
(assert (format-float nil 3.2) "3.2")

;;; test for (format-integer) ;;;
(assert (format-integer nil 100 10) "100")

;;; test for (format-object) ;;;
(assert (format-object nil "ahaha" t) "\"ahaha\"")

;;; test for (format-char) ;;;
(assert (format-char nil #\a) "a")

;;; test for cdr ;;;
(assert (cadr '(a b c d)) 'b)

;;; test for (caddr) ;;;
(assert (caddr '(a b c d)) 'c)

;;; test for (cadddr) ;;;
(assert (cadddr '(a b c d)) 'd)

;;; test for (cddr) ;;;
(assert (cddr '(a b c d e)) '(c d e))

;;; test for (cdddr) ;;;
(assert (cdddr '(a b c d e)) '(d e))

;;; test for (first) ;;;
(assert (first '(a b c d)) 'a)

;;; test for (second) ;;;
(assert (second '(a b c d)) 'b)

;;; test for (third) ;;;
(assert (third '(a b c d)) 'c)

;;; test for (rest) ;;;
(assert (rest '(a b c d)) '(b c d))

;;; test for (nth) ;;;
(assert (nth 2 '(a b c d)) 'c)

;;; test for (setf cadr)
(assert (let ((L '(a b c d))) (setf (cadr L) 'x) L) '(a x c d))

;;; test for (setf caddr)
(assert (let ((L '(a b c d))) (setf (caddr L) 'x) L) '(a b x d))

;;; test for (setf cadddr)
(assert (let ((L '(a b c d))) (setf (cadddr L) 'x) L) '(a b c x))

;;; test for (setf first)
(assert (let ((L '(a b c d))) (setf (first L) 'x) L) '(x b c d))

;;; test for (setf second)
(assert (let ((L '(a b c d))) (setf (second L) 'x) L) '(a x c d))

;;; test for (setf third)
(assert (let ((L '(a b c d))) (setf (third L) 'x) L) '(a b x d))

;;; test for (setf nth)
(assert (let ((L '(a b c d))) (setf (nth 3 L) 'x) L) '(a b c x))

;;; test for (setf cddr)
(assert (let ((L '(a b c d))) (setf (cddr L) 'x) L) '(a b . x))

;;; test for (setf cdddr)
(assert (let ((L '(a b c d))) (setf (cdddr L) 'x) L) '(a b c . x))

;; test for (find-all-string-submatch)
(assert (=~ "a(x*)b" "-ab-") '(("ab" "")))
(assert (=~ "a(x*)b" "-axxb-") '(("axxb" "xx")))
(assert (=~ "a(x*)b" "-ab-axb-") '(("ab" "")("axb" "x")))
(assert (=~ "a(x*)b" "-axxb-ab-") '(("axxb" "xx") ("ab" "")))

;; test for (find-all-string-submatch-index)
(assert (=~i "a(x*)b" "-ab-") '((1 3 2 2)))
(assert (=~i "a(x*)b" "-axxb-") '((1 5 2 4)))
(assert (=~i "a(x*)b" "-ab-axb-") '((1 3 2 2)(4 7 5 6)))
(assert (=~i "a(x*)b" "-axxb-ab-") '((1 5 2 4)(6 8 7 7)))


