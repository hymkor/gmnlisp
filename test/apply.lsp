(let ((b (create-string-output-stream)))
    (apply #'format b "~S ~S" (list 1 (cons 2 3)))
    (test (get-output-stream-string b) "1 (2 . 3)"))
