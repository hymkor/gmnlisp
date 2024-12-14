(test
  (with-handler
    (lambda (c)
      (if (instancep c <division-by-zero>)
        (continue-condition c (car (arithmetic-error-operands c)))))
    (div 4 0))
  4)
