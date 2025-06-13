(flet
  ((tst
     (x)
     (catch 'fail
            (with-handler
              (lambda (c) 
                ; (format (error-output) "~S~%" c)
                (if (instancep c (class <domain-error>))
                  (throw 'fail 'DOMAIN-ERROR)
                  (throw 'fail 'OTHER-ERROR)
                  )
                ) ; lambda
              (finish-output x)
              'NO-ERROR
              ) ; handler
            ) ; catch
     ) ; flet tst
   ) ; flet functions
  (assert-eq (tst 1) 'DOMAIN-ERROR)
  (with-open-output-file
    (w *dev-null*)
    (assert-eq (tst w) 'NO-ERROR)
    )
  )
