(lambda (newvalue source z)
  (if (stringp source)
    (string-append
      (subseq source 0 z)
      (create-string 1 newvalue)
      (subseq source (1+ z) (length source)))
    (let ((s source))
      (while s
        (if (zerop z)
          (set-car newvalue s))
        (decf z)
        (setq s (cdr s)))
      source)))
