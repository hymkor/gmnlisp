(defmacro macro-for (iters test-result &rest body)
  (let ((inits nil)
        (steps nil)
        (test (car test-result))
        (result (elt test-result 1)))
    (while iters
      (let ((e (car iters)))
        (setq inits (cons (list (car e) (elt e 1)) inits))
        (setq steps (cons `(setq ,(car e) ,(elt e 2)) steps))
        )
      (setq iters (cdr iters)))
    (setq steps (cons 'progn steps))
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
      (format t "i=~s, j=~s~%" i j)
      )

; vim:set lispwords+=while:
