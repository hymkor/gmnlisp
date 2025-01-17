(with-open-output-file
  (w "/tmp/foo")

  (format w "~&~&X~&~&Y~&")
  ); with-open-output-file


(format (standard-output) "~S~%"
  (equal
    ; result
    (with-open-input-file
      (r "/tmp/foo")
      (let ((c nil) (buf (create-string-output-stream)))
        (while (setq c (read-byte r nil nil))
          (write-byte c buf))
        (get-output-stream-string buf)))
    ; expect
    (apply #'string-append
      (mapcar (lambda (c) (create-string 1 c)) (list #\X #\newline #\Y #\newline)))
    ); equal
  ); format
