(lambda-macro (predform keyform &rest body)
  (let ((keyform_ (gensym)) (tflag nil))
    (setq body
          (mapcar
            (lambda (entry)
              (if (equal (car entry) 't)
                (progn
                  (setq tflag t)
                  (cons 't (list (cons 'progn (cdr entry)))))
                (progn
                  (if tflag
                    (simple-error "syntax error"))
                  (append
                    (list
                      (cons
                        'or
                        (mapcar
                          (lambda (key1)
                            (list 'funcall predform keyform_ key1))
                          (car entry))))
                    (list (cons 'progn (cdr entry))))))
              ) body))
    (setq body (cons 'cond body))
    (setq body `(let ((,keyform_ ,keyform)) (assure <function> ,predform) ,body))
    ;(format t "~s~%" body)
    ;body
    ))
