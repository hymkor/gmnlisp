(lambda-macro (expr &rest args) (let ((x (gensym))) `(let ((,x ,expr)) (progn ,@args) ,x)))
