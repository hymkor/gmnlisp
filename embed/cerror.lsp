(lambda (continue-string error-string :rest obj)
  (signal-condition
    (create (class <simple-error>)
        'format-string error-string
        'format-arguments obj)
    (let ((str (create-string-output-stream)))
      (apply format str continue-string obj)
      (get-output-stream-string str))))
