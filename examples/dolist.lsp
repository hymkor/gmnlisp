(defmacro dolist (pair &rest commands)
  (let ((key (car pair))
        (values (car (cdr pair))))
    `(mapc (lambda (,key) ,@commands) ,values)
    )
  )

(dolist (x '(1 2 3)) (format t "~s~%" x))
