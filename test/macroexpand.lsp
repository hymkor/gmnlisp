(defmacro dbl (x) (list '+ x x))
(test 
  (macroexpand '(dbl (incf a1)))
  '(+ (incf a1) (incf a1)))
