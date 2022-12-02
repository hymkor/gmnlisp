(lambda-macro (expr1 expr2 &rest args) `(progn ,expr1 (prog1 ,expr2 ,@args)))
