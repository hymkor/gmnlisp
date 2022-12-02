(lambda (newvalue z source) (let ((s source)) (while s (setq z (1- z)) (if (zerop z) (set-cdr newvalue s)) (setq s (cdr s))) source))
