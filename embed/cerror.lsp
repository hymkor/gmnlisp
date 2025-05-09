(lambda (continue-string error-string :rest obj)
  (signal-condition
    (%make-simple-error error-string obj)
    (let ((str (create-string-output-stream)))
      (apply #'format str continue-string obj)
      (get-output-stream-string str))))
