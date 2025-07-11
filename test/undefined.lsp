(assert-eq
  (catch
    'c
     (with-handler
       (lambda (c)
         (if (and (instancep c (class <unbound-variable>))
                  (equal (undefined-entity-name c) 'undefined)
                  (equal (undefined-entity-namespace c) 'variable))
           (throw 'c "OK")
           "NG1"))
       (+ 1 undefined))
     "NG2")
  "OK")

;;; test for (with-handler)
(assert-eq (catch 'hoge
             (with-handler
               (lambda (c)
                 (if (and (instancep c (class <undefined-function>))
                          (eql (undefined-entity-name c) 'not-exist-func)
                          (eql (undefined-entity-namespace c) 'function))
                   (throw 'hoge "OK")))
               (not-exist-func)
               "NG"
               )
             )
      "OK")
