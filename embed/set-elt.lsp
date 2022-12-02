(lambda-macro (newvalue seq &rest z) `(if (arrayp ,seq) (set-aref ,newvalue ,seq ,@z) (setf ,seq (swap-elt ,newvalue ,seq ,@z))))
