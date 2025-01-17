(assert-eq
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
(assert-eq (fibo2 10) 55)

(assert-eq
  (for ((vec (vector 0 0 0 0 0)) 
        (i 0 (+ i 1))) 
    ((= i 5) vec) 
    (setf (elt vec i) i))
  #(0 1 2 3 4))

(assert-eq
  (let ((x '(1 3 5 7 9))) 
    (for ((x x (cdr x)) 
          (sum 0 (+ sum (car x)))) 
      ((null x) sum))) 
  25)
