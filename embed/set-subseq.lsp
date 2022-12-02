(lambda-macro (newvalue seq start end)
  `(setf ,seq (swap-subseq ,seq ,start ,end ,newvalue)))
