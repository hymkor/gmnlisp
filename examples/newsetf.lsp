(defun swap-elt (source z newvalue)
  (cond
    ((stringp source)
     (string-append
       (subseq source 0 z)
       (create-string 1 newvalue)
       (subseq source (1+ z) (length source))))
    (t
      (let ((result nil))
        (while source
          (setq result
                (cons (if (zerop z)
                        newvalue
                        (car source)) result))
          (setq z (1- z))
          (setq source (cdr source))
          )
        (nreverse result))
      )
    )
  )
(defmacro setf (expr newvalue)
  (if (symbolp expr)
    `(setq ,expr ,newvalue)
    (case (car expr)
      (('car)
       (let ((cns (elt expr 1)))
         `(setf ,cns (cons ,newvalue (cdr ,cns)))))
      (('cdr)
       (let ((cns (elt expr 1)))
         `(setf ,cns (cons (car ,cns) ,newvalue))))
      (('elt)
       (let ((seq (elt expr 1)) (z (elt expr 2)))
         `(setf ,seq (swap-elt ,seq ,z ,newvalue))
         ))
      (t
        (error))
      ) ; case
    ); if
  ) ; defmacro

(let ((a nil))
  (setf a '(1 2 3))
  (format t "~s~%" a) ; (1 2 3)
  (setf (car a) 9)
  (format t "~s~%" a) ; (9 2 3)
  (setf (cdr a) '(8 7))
  (format t "~s~%" a) ; (9 8 7)
  (setf (elt a 1) 0)
  (format t "~s~%" a) ; (9 0 7)
  (setq a "あいうえお")
  (setf (elt a 2) #\ん)
  (format t "~s~%" a) ;
  (setq a '("ahaha" "ihihi"))
  (setf (elt (car a) 1) #\あ)
  (format t "~s~%" a) ;
  )

; vim:set lispwords+=while:
