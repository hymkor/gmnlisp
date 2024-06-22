(defclass <simple-error> (<error>)
  ((format-string
    :initarg  format-string
    :reader simple-error-format-string)
   (format-arguments
    :initarg  format-arguments
    :reader simple-error-format-arguments)))
