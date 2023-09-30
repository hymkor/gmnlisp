(let ((A (create-array '(3 2) 1)))
  (test (array-dimensions A) '(3 2))
  (test (basic-array-p A) t)
  (set-aref 2 A 1 1)
  (set-aref (create-array '(2) 4) A 0)
  (setf (elt A 2 1) "SETF")
  (test (elt A 0 0) 4)
  (test (elt A 0 1) 4)
  (test (elt A 0) (create-array '(2) 4))
  (test (elt A 1 1) 2)
  (test (elt A 1 0) 1)
  (test (elt A 2 1) "SETF")
  )

;;; test for the constructor for array ;;;
(let ((A #2a( (1 2 3) (4 5 6) )))
  (test (array-dimensions A) '(2 3))
  (test (aref A 0 0) 1)
  (test (aref A 0 1) 2)
  (test (aref A 0 2) 3)
  (test (aref A 1 0) 4)
  (test (aref A 1 1) 5)
  (test (aref A 1 2) 6)

  (setf (aref A 1 0) 44)
  (test (aref A 1 0) 44)
  (setf (elt A 1 0) 77)
  (test (elt A 1 0) 77)
  )

(labels ((p (x) (list (basic-array-p x)
                      (basic-array*-p x)
                      (general-array*-p x))))
  (test (p '(a b c))          '(nil nil nil))
  (test (p "abc"   )          '(t   nil nil))
  (test (p '#1a(a b c))       '(t   nil nil))
  (test (p '#(a b c))         '(t   nil nil))
  (test (p '#2a((a) (b) (c))) '(t   t   t  ))
  )
(test (mapcar (lambda (x)
                (list (basic-array-p x)
                      (basic-array*-p x)
                      (general-array*-p x)))
              '((a b c)
                "abc"
                #(a b c)
                #1a(a b c)
                #2a((a) (b) (c))))
'((nil nil nil) (t nil nil) (t nil nil) (t nil nil) (t t t)))