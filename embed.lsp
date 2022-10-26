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
      (('dynamic)
       (let ((name (elt expr 1)))
         `(defdynamic ,name ,newvalue))
       )
      (t
        (error))
      ) ; case
    ); if
  ) ; defmacro
; vim:set lispwords+=while:
