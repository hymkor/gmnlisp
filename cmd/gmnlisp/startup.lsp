(defmacro assert-eq (source expect)
  (let ((result (gensym)))
    `(let ((,result ,source))
       (if (not (equal ,result ,expect))
         (progn
           (format (error-output)
                   "~&Failed: ~S, which should have been ~S, was ~S~%"
                   (quote ,source)
                   ,expect
                   ,result)
            (abort))
         ))))
