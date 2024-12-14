(test
  'ok
  (block main
      (with-handler
          (lambda (c)
              (return-from main 'ok))
          (with-handler
              (lambda (c))
              (/ 2 0)
          )
      )
  ))
