(lambda-macro (predform keyform &rest body)
  (let ((keyform_ (gensym)))
    (setq body
          (mapcar
            (lambda (entry)
              (if (equal (car entry) 't)
                entry
                (append
                  (list
                    (cons
                      'or
                      (mapcar
                        (lambda (key1)
                          (list 'funcall predform keyform_ key1))
                        (car entry))))
                      (cdr entry)))
                  ) body))
    (setq body (cons 'cond body))
    (setq body `(let ((,keyform_ ,keyform)) ,body))
    ;(format t "~s~%" body)
    ;body
    ))
