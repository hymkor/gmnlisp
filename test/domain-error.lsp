; <string>
(assert-eq
  (catch
    'c
    (with-handler
      (lambda (c)
        ;(format (standard-output) "condition=~S~%~S" c (class-of c))
        (if (instancep c (class <domain-error>))
          (throw 'c "OK")
          (throw 'c "NOT DOMAIN ERROR")))
      (format (standard-output) 1))
    "NOT HANDLED")
  "OK")

; <integer>
(assert-eq
  (catch
    'c
    (with-handler
      (lambda (c)
        ;(format (standard-output) "condition=~S~%~S" c (class-of c))
        (if (instancep c (class <domain-error>))
          (throw 'c "OK")
          (throw 'c "NOT DOMAIN ERROR")))
      (format (standard-output) (+ 1 "x")))
    "NOT HANDLED")
  "OK")
; <float>
(assert-eq
  (catch
    'c
    (with-handler
      (lambda (c)
        ;(format (standard-output) "condition=~S~%~S" c (class-of c))
        (if (instancep c (class <domain-error>))
          (throw 'c "OK")
          (throw 'c "NOT DOMAIN ERROR")))
      (format (standard-output) (+ 1.0 "x")))
    "NOT HANDLED")
  "OK")
; <output-stream-writer>
(assert-eq
  (catch
    'c
    (with-handler
      (lambda (c)
        (if (instancep c (class <domain-error>))
          (throw 'c "OK")
          (throw 'c "NOT DOMAIN ERROR")))
      (format "HOGE" "FUGA"))
    "NOT HANDLED")
  "OK")
(catch
  'c
  (with-handler
    (lambda (con)
      (if (instancep con (class <domain-error>))
        (continue-condition con 'newfunc)))
    (defun 1111 (v)
      (+ v 1))))
(assert-eq
  (newfunc 1)
  2)

