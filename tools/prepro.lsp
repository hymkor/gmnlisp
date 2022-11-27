(defun string-replace (s from to)
  (let ((index nil) (buffer (create-string-output-stream)))
    (while (setq index (string-index from s))
      (format-object buffer (subseq s 0 index) nil)
      (format-object buffer to nil)
      (setq s (subseq s (+ index (length from)) (length s)))
      )
    (format-object buffer s nil)
    (get-output-stream-string buffer)))

(defun to-safe (s)
  (setq s (string-replace s "\\" "\\\\"))
  (string-replace s "\"" "\\\""))

(let ((nowlisp nil) (index nil) (line nil) (b (create-string-output-stream)))
  (while (setq line (read-line (standard-input) nil nil))
    (while line
      (if nowlisp
        (progn
          (if (setq index (string-index "%>" line))
            (progn
              (format b "~a" (subseq line 0 index))
              (setq line (subseq line (+ index 2) (length line)))
              (if (<= (length line) 0) (setq line nil))
              (setq nowlisp nil)
              )
            (progn
              (format b "~a~%" line)
              (setq line nil)
              )
            )
          )
        (progn ; not nowlisp
          (if (setq index (string-index "<%" line))
            (progn
              (format b "(format-object t \"~a\" nil)~%" 
                      (to-safe (subseq line 0 index)))
              (setq line (subseq line (+ index 2) (length line)))
              (if (<= (length line) 0) (setq line nil))
              (setq nowlisp t)
              )
            (progn
              (format b "(format t \"~~a~~%\" \"~a\")~%"
                      (to-safe line))
              (setq line nil)
              )
            )
          ); progn
        )
      )
    )
  (setq b (get-output-stream-string b))
  (setq b (create-string-input-stream b))
  (let ((ss nil) (s nil))
    (while (setq s (read b nil nil))
      (setq ss (append ss (list s))))
    (apply #'progn ss)))
