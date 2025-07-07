(lambda (des fn :rest seq)
  (assure <function> fn)
  (labels
    ((elts (sq i) (mapcar (lambda (s) (elt s i)) sq)))

    (let ((i 0)
          (n (apply #'min (mapcar #'length (cons des seq)))))
      (while (< i n)
        (setf (elt des i)
              (apply fn (elts seq i)))
        (setq i (+ i 1)))))
  des)
