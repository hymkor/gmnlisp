;;; test for (list)
(test (list 1 2 3 4) '(1 2 3 4))

;;; test for (listp)
(test (listp ()) t)
(test (listp 1) nil)
(test (listp '(1 2 3)) t)

;;; test for reverse
(test (reverse '(1 2 3 4))
      '(4 3 2 1))

;;; test for nreverse
(test (nreverse '(1 2 3 4))
      '(4 3 2 1))

;;; test for (append)
(test (append '(1 2) '(3 4))
      '(1 2 3 4))
(test (append '(1 2) '(3 4) '(5 6))
      '(1 2 3 4 5 6))
(test (append '() '(1 2) '(3 4))
      '(1 2 3 4))

;append do not destruct original not last list.
(test (let ((x '(1 2 3)))
        (append x '(4 5 6))
        x)
      '(1 2 3))

;;; apend destruct the last list only
(let* ((part1 (list 1 2 3))
       (part2 (list 4 5 6))
       (part3 (list 7 8 9))
       (all (append part1 part2 part3)))
  (setf (elt all 1) "aaa")
  (setf (elt all 4) "bbb")
  (setf (elt all 7) "ccc")
  (test part1 '(1 2 3))
  (test part2 '(4 5 6))
  (test part3 '(7 "ccc" 9)))

(test (append '() '(1)) '(1))

