(test
  (with-handler
    (lambda (c)
      (if (instancep c <division-by-zero>)
        (continue-condition c 4)))
    (/ 4 0))
  4)
