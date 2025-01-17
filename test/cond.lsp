(assert-eq
  (block main
    (with-handler
      (lambda (c)
        (if (instancep c (class <division-by-zero>))
          (return-from main 'ok)
          (return-from main (class-of c))))
        (div 2 0)))
  'ok)
