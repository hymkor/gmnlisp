(lambda-macro (test &rest args) `(if ,test nil (progn ,@args)))
