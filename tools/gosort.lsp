(labels
  ((min-index
     (source)
     (let ((value (car source)) (min-i 0) (i 1))
       (mapc
         (lambda (e)
           (cond ((string< e value)
                  (setq value e)
                  (setq min-i i)))
           (setq i (+ i 1)))
         (cdr source))
       min-i))

   (sort-strings
     (source)
     (while (cdr source)
       (let ((index (min-index source))
             (tmp (car source)))
         (setf (car source) (elt source index))
         (setf (elt source index) tmp))
       (setq source (cdr source))))
   )

  (let
    ((sorting nil) (buffer nil) (line nil))
    (while (setq line (read-line (standard-input) nil nil))
      (if sorting
        (progn
          (if (string-index "*sort*end*" line)
            (progn
              (sort-strings buffer)
              (mapc (lambda (x) (format (standard-output) "~a~%" x nil)) buffer)
              (setq sorting nil)
              (format (standard-output) "~a~%" line))
            (setq buffer (cons line buffer))))
        (progn
          (format (standard-output) "~a~%" line)
          (if (string-index "*sort*start*" line)
            (progn
              (setq sorting t)
              (setq buffer nil))))))))
