(assert-eq
  (with-handler
    (lambda (c)
      (if (instancep c (class <division-by-zero>))
        (continue-condition c (car (arithmetic-error-operands c)))))
    (div 4 0))
  4)
