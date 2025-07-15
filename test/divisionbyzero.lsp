(assert-eq
  (catch
    'arith
    (with-handler
      (lambda (c)
        (cond 
          ((not (instancep c (class <division-by-zero>)))
           (throw 'arith 'FAIL1))
          ((not (equal (arithmetic-error-operation c) #'div))
           (format t "~S != ~S~%" (arithmetic-error-operation c) #'div)
           (throw 'arith 'FAIL2))
          ((not (equal (car (arithmetic-error-operands c)) 4))
           (throw 'arith 'FAIL3))
          (t
           (throw 'arith 'PASS))
          ))
      (div 4 0)
      )
    )
  'PASS)
