(lambda-macro (e &rest body)
  (let (result ee)
    (while e
      (setq ee (car e))
      (setq result (cons (cons (car ee) (list (cons 'lambda (cdr ee)))) result))
      (setq e (cdr e)))
    (setq result (nreverse result))
    `(let* ,result ,@body)))
