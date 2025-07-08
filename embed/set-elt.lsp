(lambda-macro (newvalue seq &rest z)
  (cond
    ((stringp seq)
     `(progn ; for verification test
        (assure <character> ,newvalue)
        (elt ,seq ,@z)
        (error "setf can not change constants"))
     )
    (t
      (let ((_newvalue (gensym)))
        `(if (general-array-p ,seq)
           (set-garef ,newvalue ,seq ,@z)
           (let ((,_newvalue ,newvalue))
             (setf ,seq (swap-elt ,_newvalue ,seq ,@z))
             ,_newvalue)))
      )
    )
  )
