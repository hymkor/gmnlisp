(lambda-macro (newvalue seq z)
  (cond
    ((stringp seq)
     `(progn ; for verification test
        (assure <character> ,newvalue)
        (elt ,seq ,z)
        (error "setf can not change constants"))
     )
    (t
      (let ((_newvalue (gensym)) (_seq (gensym)) (_z (gensym)))
        `(let ((,_newvalue ,newvalue) (,_seq ,seq) (,_z ,z))
           (assure <integer> ,_z)
           (cond
             ((< ,_z 0)
              (%raise-domain-error ,_seq "not a non-negative integer"))

             ((general-vector-p ,_seq)
              (set-garef ,_newvalue ,seq ,_z))

             ((or (listp ,_seq) (stringp ,_seq))
               (setf ,seq (swap-elt ,_newvalue ,_seq ,_z))
               ,_newvalue)

             (t
               (%raise-domain-error ,_seq "not a sequence or a list")
               )
             ))))))
