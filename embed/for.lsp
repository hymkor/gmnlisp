(lambda-macro (iters test-result &rest body)
  (let ((inits nil)
        (steps '(psetq))
        (test (car test-result))
        (result (append '(progn) (cdr test-result))))
    (while iters
      (let* ((iteration-spec (car iters))
             (var (car iteration-spec))
             (init (elt iteration-spec 1))
             (iteration-spec-length (length iteration-spec)))
        (setq inits (cons (list var init) inits))
        (cond
          ((= iteration-spec-length 3)
           (let ((step (elt iteration-spec 2)))
             (setq steps (append steps (list var step)))))
          ((/= iteration-spec-length 2)
           (error "FOR: too few arguments")))
        (setq iters (cdr iters))
        ) ; let*
        ) ; while
      (if (> (length steps) 1)
        `(let ,inits (while (not ,test) ,@body ,steps) ,result)
        `(let ,inits (while (not ,test) ,@body) ,result))
      ) ; let
    ) ; lambda
