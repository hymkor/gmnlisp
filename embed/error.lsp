(lambda (error-string :rest obj)
  (assure <string> error-string)
  (signal-condition
    (%make-simple-error error-string obj)
    nil))
