(defmacro newsetf (expr newvalue)
  (cond
    ((symbolp expr)
     `(setq ,expr ,newvalue))
    (t
      (case (car expr)
        (('car)
         (let ((cns (elt expr 1)))
           `(setq ,cns (cons ,newvalue (cdr ,cns)))))
        (('cdr)
         (let ((cns (elt expr 1)))
           `(setq ,cns (cons (car ,cns) ,newvalue))))
        (('elt)
         (let ((arg1 (elt expr 1)) (arg2 (elt expr 2)))
           `(let ((result nil) (source ,arg1) (count ,arg2) )
              (while source
                (setq result
                      (append result
                              (list (if (equal count 0)
                                      newvalue
                                      (car source)))))
                (setq count (1- count))
                (setq source (cdr source)))
              (setq ,arg1 result))))
        (t
          (error))
        ) ; case
      ) ; t
    ) ; cond
  ) ; defmacro

(let ((a nil))
  (newsetf a '(1 2 3))
  (format t "~s~%" a) ; (1 2 3)
  (newsetf (car a) 9)
  (format t "~s~%" a) ; (9 2 3)
  (newsetf (cdr a) '(8 7))
  (format t "~s~%" a) ; (9 8 7)
  (newsetf (elt a 1) 0)
  (format t "~s~%" a) ; (9 0 7)
  )

; vim:set lispwords+=while:
