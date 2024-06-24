; <string>
(test
  (catch
    'c
    (with-handler
      (lambda (c)
        ;(format (standard-output) "condition=~S~%~S" c (class-of c))
        (if (instancep c <domain-error>)
          (throw 'c "OK")
          (throw 'c "NOT DOMAIN ERROR")))
      (format (standard-output) 1))
    "NOT HANDLED")
  "OK")

; <integer>
(test
  (catch
    'c
    (with-handler
      (lambda (c)
        ;(format (standard-output) "condition=~S~%~S" c (class-of c))
        (if (instancep c <domain-error>)
          (throw 'c "OK")
          (throw 'c "NOT DOMAIN ERROR")))
      (format (standard-output) (+ 1 "x")))
    "NOT HANDLED")
  "OK")
; <float>
(test
  (catch
    'c
    (with-handler
      (lambda (c)
        ;(format (standard-output) "condition=~S~%~S" c (class-of c))
        (if (instancep c <domain-error>)
          (throw 'c "OK")
          (throw 'c "NOT DOMAIN ERROR")))
      (format (standard-output) (+ 1.0 "x")))
    "NOT HANDLED")
  "OK")
