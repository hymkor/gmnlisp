(test
  (catch
    'c
     (with-handler
       (lambda (c)
         (if (and (instancep c <unbound-variable>)
                  (equal (undefined-entity-name c) 'undefined)
                  (equal (undefined-entity-namespace c) 'variable))
           (throw 'c "OK")
           "NG1"))
       (+ 1 undefined))
     "NG2")
  "OK")

;;; test for (with-handler)
(test (catch 'hoge
             (with-handler
               (lambda (c)
                 (if (and (instancep c <undefined-function>)
                          (eql (undefined-entity-name c) 'not-exist-func)
                          (eql (undefined-entity-namespace c) 'function))
                   (throw 'hoge "OK")))
               (not-exist-func)
               "NG"
               )
             )
      "OK")
