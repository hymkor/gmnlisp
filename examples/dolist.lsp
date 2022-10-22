(defmacro dolist (pair &rest commands)
  (let ((key (car pair))
        (_values (car (cdr pair))))
    `(let ((VALUES ,_values)
           (,key nil))
       (while
         VALUES

         (setq ,key (car VALUES))
         (setq VALUES (cdr VALUES))

         ,@commands
         )
       )
    )
  )

(dolist (x '(1 2 3)) (format t "~s~%" x))
