(lambda (error-string :rest obj)
  (signal-condition
    (create (class <simple-error>)
      'format-string error-string
      'format-arguments obj)
    nil))
