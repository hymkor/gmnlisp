(labels
  ((print-lines
     (node indent)
     (let ((L (car node)))
       (format t "(~s" L)
       (while (setq node (cdr node))
         (unless (equal L ',)
           (format t "~%~a" (string-append "  " indent)))
         (print (setq L (car node)) (string-append "  " indent)))
       (format t ")")))
   (print
     (node indent)
     (if (not (consp node))
       (format-object t node t)
       (case (car node)
         (('let 'let* 'flet 'labels 'lambda 'lambda-macro
           'if 'progn 'while 'dolist 'defun 'defmacro)
          (print-lines node indent)
          )
         (('backquote)
          (format t "`")
          (print-lines (cadr node) indent)
          )
         (t
           (format-object t node t)
           )
         )))
   (print-source
     (fd)
     (let (node)
       (while (setq node (read fd nil nil))
         (print node ""))
       (format t "~%"))))
  (if *posix-argv*
    (dolist (fname *posix-argv*)
      (with-open-input-file
        (fd fname)
        (print-source fd)))
    (print-source (standard-input))))
