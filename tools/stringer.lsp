(let ((b (standard-output))
      (package-name (car *posix-argv*))
      (arguments (cdr *posix-argv*)))
  (format b "package ~a~%" package-name)
  (format b "~%")
  (format b "import (~%")
  (format b "~a\"strings\"~%" #\tab)
  (format b ")~%")
  (dolist (theType arguments)
    (format b "~%")
    (format b "func (t ~a) String() string {~%" theType)
    (format b "~avar buffer strings.Builder~%" #\tab)
    (format b "~at.PrintTo(&buffer, PRINC)~%" #\tab)
    (format b "~areturn buffer.String()~%" #\tab)
    (format b "}~%")
    (format b "~%")
    (format b "func (t ~a) GoString() string {~%" theType)
    (format b "~avar buffer strings.Builder~%" #\tab)
    (format b "~at.PrintTo(&buffer, PRINT)~%" #\tab)
    (format b "~areturn buffer.String()~%" #\tab)
    (format b "}~%")))
