(with-open-output-file
  (w (string-append *temp-dir* "/foo"))

  (format w "~&~&X~&~&Y~&")
  ); with-open-output-file


(assert-eq
  ; result
  (with-open-input-file
    (r (string-append *temp-dir* "/foo"))
    (let ((c nil) (buf (create-string-output-stream)))
      (while (setq c (read-byte r nil nil))
        (write-byte c buf))
      (get-output-stream-string buf)))

  ; expect
  (apply #'string-append
         (mapcar (lambda (c) (create-string 1 c))
                 (list #\X #\newline #\Y #\newline)))
  ); test
