(defmacro dbl (x) (list '+ x x))
(assert-eq 
  (macroexpand '(dbl (incf a1)))
  '(+ (incf a1) (incf a1)))
