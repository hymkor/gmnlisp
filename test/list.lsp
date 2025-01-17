;;; test for (list)
(assert-eq (list 1 2 3 4) '(1 2 3 4))

;;; test for (listp)
(assert-eq (listp ()) t)
(assert-eq (listp 1) nil)
(assert-eq (listp '(1 2 3)) t)

;;; test for reverse
(assert-eq (reverse '(1 2 3 4))
      '(4 3 2 1))

;;; test for nreverse
(assert-eq (nreverse '(1 2 3 4))
      '(4 3 2 1))

;;; test for (append)
(assert-eq (append '(1 2) '(3 4))
      '(1 2 3 4))
(assert-eq (append '(1 2) '(3 4) '(5 6))
      '(1 2 3 4 5 6))
(assert-eq (append '() '(1 2) '(3 4))
      '(1 2 3 4))

;append do not destruct original not last list.
(assert-eq (let ((x '(1 2 3)))
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
  (assert-eq part1 '(1 2 3))
  (assert-eq part2 '(4 5 6))
  (assert-eq part3 '(7 "ccc" 9)))

(assert-eq (append '() '(1)) '(1))

;;; (create-list)
(assert-eq (create-list 3 17) '(17 17 17))
(assert-eq (create-list 2 #\a) '(#\a #\a))
