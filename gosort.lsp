(defun sort-strings (source)
  (let ((result nil)
        (maximum nil)
        (newsource nil))
    (while source
      (setq maximum nil)
      (setq newsource nil)
      (mapc (lambda (x)
                (cond
                  ((null maximum)
                   (setq maximum x))
                  ((string> x maximum)
                   (setq newsource (cons maximum newsource))
                   (setq maximum x))
                  (t
                    (setq newsource (cons x newsource)))))
              source)
      (setq result (cons maximum result))
      (setq source newsource)
    )
    result
  )
)
(let
  ((sorting nil)
   (buffer nil)
   (line nil)
   (in (standard-input))
   (out (standard-output)))
  (while (setq line (read-line in nil nil))
    (if sorting
      (progn
        (if (string-index "*sort*end*" line)
          (progn
            (mapc (lambda (x) (format out "~a~%" x nil))
                  (sort-strings buffer))
            (setq sorting nil)
            (format out "~a~%" line))
          (setq buffer (cons line buffer))))
      (progn
        (format out "~a~%" line)
        (if (string-index "*sort*start*" line)
          (setq sorting t
                buffer '())))
    ) ; if
  ) ; while
) ; let
