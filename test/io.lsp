(test
  (with-standard-input (create-string-input-stream "this is a string")
    (list (read) (read)))
  '(this is))
