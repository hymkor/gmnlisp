(defmacro macro-for (iters test-result &rest body)
  (let ((inits nil)
        (steps nil)
        (test (car test-result))
        (result (elt test-result 1)))
    (while iters
      (let ((e (car iters)))
        (setq inits (cons (list (car e) (elt e 1)) inits))
        (setq steps (append steps (list (car e) (elt e 2)))))
      (setq iters (cdr iters)))
    (setq steps (cons 'psetq steps))
    (format t "inits=~s~%" inits)
    (format t "steps=~s~%" steps)
    (format t "test=~s~%" test)
    (format t "result=~s~%" result)
    `(let ,inits
       (while (not ,test)
         ,@body
         ,steps)
       ,result)))

(macro-for ((i 0 (1+ i)) (j 10 (1- j))) ((= i 5) i)
           (format t "i=~s, j=~s~%" i j))

(defmacro assert (func expect)
  (let ((result (gensym)))
    `(let ((,result ,func))
       (if (not (equal ,result ,expect))
         (progn
           (format t "Failed: ~s~%" (quote ,func))
           (format t "  expect: ~s but ~s~%" ,expect ,result)
           (abort))))))

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
          (b 1 (+ a b))
          )
         ((<= n 0) a))))
(assert (fibo2 10) 55)
