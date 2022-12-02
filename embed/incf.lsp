(lambda-macro (place &rest args) (let ((delta (if args (car args) 1))) `(setf ,place (+ ,place ,delta))))
