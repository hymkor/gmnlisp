(lambda (error-string :rest obj)
  (signal-condition
    (%make-simple-error error-string obj)
    nil))
