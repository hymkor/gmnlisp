(block test
  (with-handler
    (lambda (c)
      (assert-eq (class-of c) (class <control-error>))
      (return-from test nil))
    (unwind-protect
      1
      (quit)
      (format (standard-error) "(quit) in (unwind-protect) did not raise an error~%"))))

(block test
  (with-handler
    (lambda (c)
      (assert-eq (class-of c) (class <control-error>))
      (return-from test nil))
    (unwind-protect
      1
      (abort)
      (format (standard-error) "(abort) in (unwind-protect) did not raise an error~%"))))
