(lambda-macro (test &rest args) `(if ,test (progn ,@args)))
