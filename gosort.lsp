(defun min-index (source)
  (let ((value (car source)) (min-index 0) (index 1))
    (while (setq source (cdr source))
      (if (string< (car source) value)
        (setq value (car source)
              min-index index))
      (incf index))
    min-index))

(defmacro swapf (left right)
  (let ((tmp (gensym)))
    `(let ((,tmp ,left))
       (setf ,left ,right)
       (setf ,right ,tmp))))

(defun sort-strings (source)
  (let ((index nil))
    (while (cdr source)
      (setq index (min-index source))
      (swapf (car source) (elt source index))
      (setq source (cdr source)))))

(let
  ((sorting nil) (buffer nil) (line nil))
  (while (setq line (read-line (standard-input) nil nil))
    (if sorting
      (progn
        (if (string-index "*sort*end*" line)
          (progn
            (sort-strings buffer)
            (dolist (x buffer)
              (format t "~a~%" x nil))
            (setq sorting nil)
            (format t "~a~%" line))
          (setq buffer (cons line buffer))))
      (progn
        (format t "~a~%" line)
        (if (string-index "*sort*start*" line)
          (setq sorting t
                buffer '()))))))
