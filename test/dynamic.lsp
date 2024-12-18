(dynamic-let
  ((a 1) (b 2))

  (test (dynamic a) 1)
  (test (dynamic b) 2)

  (dynamic-let
    ((a 3) (b 4))

    (test (dynamic a) 3)
    (test (dynamic b) 4))

  (test (dynamic a) 1)
  (test (dynamic b) 2))

(test
  (catch
    'c
    (with-handler
      (lambda (c)
        (if (and (instancep c (class <undefined-entity>))
                 (instancep c (class <unbound-variable>))
                 (equal (class-of c) (class <unbound-variable>)))
          (let ((ns (undefined-entity-namespace c)))
            (if (equal ns 'dynamic-variable)
              (throw 'c "OK")
              (throw 'c (format nil "NG: undefined-entity-namespace: ~S" ns))))
          (throw 'c (format nil "NG: ~S" (class-of c)))))
      (dynamic unset-variable)))
  "OK")
