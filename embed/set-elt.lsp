(lambda-macro (newvalue seq &rest z)
  `(if (basic-array-p ,seq)
     (set-aref ,newvalue ,seq ,@z)
     (setf ,seq (swap-elt ,newvalue ,seq ,@z))))
