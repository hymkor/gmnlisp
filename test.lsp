(defmacro assert (&rest args)
  (unless (equal (length args) 2)
    (format t "Too few arguments: ~s~%" args)
    (abort)
    )
  (let ((func (car args)) (expect (cadr args)) (result (gensym)))
    `(let ((,result ,func))
       (if (not (equal ,result ,expect))
         (progn
           (format t "Failed: ~s~%" (quote ,func))
           (format t "  expect: ~s but ~s~%" ,expect ,result)
           (abort))))))

;;; test for create-array ;;;
(let ((A (create-array '(3 3) 0)))
  (assert (aref A 1 1) 0)
  (set-aref 2 A 1 1)
  (assert (aref A 1 1) 2)
  ;(format t "~s~%" (aref A 1))
  )

(let ((A (create-array '(3 2) 1)))
  (assert (array-dimensions A) '(3 2))
  (assert (arrayp A) t)
  (set-aref 2 A 1 1)
  (set-aref (create-array '(2) 4) A 0)
  (setf (elt A 2 1) "SETF")
  (assert (elt A 0 0) 4)
  (assert (elt A 0 1) 4)
  (assert (elt A 0) (create-array '(2) 4))
  (assert (elt A 1 1) 2)
  (assert (elt A 1 0) 1)
  (assert (elt A 2 1) "SETF")
  )

;;; test for the constructor for array ;;;
(let ((A #2a( (1 2 3) (4 5 6) )))
  (assert (array-dimensions A) '(2 3))
  (assert (aref A 0 0) 1)
  (assert (aref A 0 1) 2)
  (assert (aref A 0 2) 3)
  (assert (aref A 1 0) 4)
  (assert (aref A 1 1) 5)
  (assert (aref A 1 2) 6)

  (setf (aref A 1 0) 44)
  (assert (aref A 1 0) 44)
  (setf (elt A 1 0) 77)
  (assert (elt A 1 0) 77)
  )

;;; test for (tagbody) and (go)
; forward test
(let ((step 0))
  (tagbody
    (setq step 1)
    ;(format t "step-1~%")
    (go skip)
    ;(format t "step-2~%")
    (setq step 2)
    skip
    )
  (assert step 1))

; backword test
(let ((step 0))
  (assert
    (block
      break1
      (tagbody
        loop-tag
        ;(format t "step=~a~%" step)
        (if (> step 4)
          (return-from break1 step))
        (incf step)
        (go loop-tag)
        )
      )
    5))

;;; test for (psetq) ;;;
(let ((x 1) (y 2))
  (psetq x y
         y x)
  (assert x 2)
  (assert y 1))

;;; test for (when) ;;;
(assert (when t 1 2 3) 3)
(assert (when nil 1 2 3) nil)

;;; test for (unless) ;;;
(assert (unless t 1 2 3) nil)
(assert (unless nil 1 2 3) 3)

;;; test for (prog1) ;;;
(assert (prog1 1 2 3 4) 1)
(assert (prog2 1 2 3 4) 2)

;;;; test for (for) ;;;
(assert
  (let (x y)
    (for ((x 0 (1+ x)) (y 0 (+ y 10)))
         ((= x 5) (+ x y))
         ))
  55)

(defun fibo2 (n)
  (let (a b)
    (for ((n n (- n 1))
          (a 0 b)
          (b 1 (+ a b)))
         ((<= n 0) a))))
(assert (fibo2 10) 55)

;;; test for (format-float) ;;;
(assert (format-float nil 3.2) "3.2")

;;; test for (format-integer) ;;;
(assert (format-integer nil 100 10) "100")

;;; test for (format-object) ;;;
(assert (format-object nil "ahaha" t) "\"ahaha\"")

;;; test for (format-char) ;;;
(assert (format-char nil #\a) "a")

;;; test for cdr ;;;
(assert (cadr '(a b c d)) 'b)

;;; test for (caddr) ;;;
(assert (caddr '(a b c d)) 'c)

;;; test for (cadddr) ;;;
(assert (cadddr '(a b c d)) 'd)

;;; test for (cddr) ;;;
(assert (cddr '(a b c d e)) '(c d e))

;;; test for (cdddr) ;;;
(assert (cdddr '(a b c d e)) '(d e))

;;; test for (first) ;;;
(assert (first '(a b c d)) 'a)

;;; test for (second) ;;;
(assert (second '(a b c d)) 'b)

;;; test for (third) ;;;
(assert (third '(a b c d)) 'c)

;;; test for (rest) ;;;
(assert (rest '(a b c d)) '(b c d))

;;; test for (nth) ;;;
(assert (nth 2 '(a b c d)) 'c)

;;; test for (setf cadr)
(assert (let ((L '(a b c d))) (setf (cadr L) 'x) L) '(a x c d))

;;; test for (setf caddr)
(assert (let ((L '(a b c d))) (setf (caddr L) 'x) L) '(a b x d))

;;; test for (setf cadddr)
(assert (let ((L '(a b c d))) (setf (cadddr L) 'x) L) '(a b c x))

;;; test for (setf first)
(assert (let ((L '(a b c d))) (setf (first L) 'x) L) '(x b c d))

;;; test for (setf second)
(assert (let ((L '(a b c d))) (setf (second L) 'x) L) '(a x c d))

;;; test for (setf third)
(assert (let ((L '(a b c d))) (setf (third L) 'x) L) '(a b x d))

;;; test for (setf nth)
(assert (let ((L '(a b c d))) (setf (nth 3 L) 'x) L) '(a b c x))

;;; test for (setf cddr)
(assert (let ((L '(a b c d))) (setf (cddr L) 'x) L) '(a b . x))

;;; test for (setf cdddr)
(assert (let ((L '(a b c d))) (setf (cdddr L) 'x) L) '(a b c . x))

;; test for (find-all-string-submatch)
(assert (=~ "a(x*)b" "-ab-") '(("ab" "")))
(assert (=~ "a(x*)b" "-axxb-") '(("axxb" "xx")))
(assert (=~ "a(x*)b" "-ab-axb-") '(("ab" "")("axb" "x")))
(assert (=~ "a(x*)b" "-axxb-ab-") '(("axxb" "xx") ("ab" "")))

;; test for (find-all-string-submatch-index)
(assert (=~i "a(x*)b" "-ab-") '((1 3 2 2)))
(assert (=~i "a(x*)b" "-axxb-") '((1 5 2 4)))
(assert (=~i "a(x*)b" "-ab-axb-") '((1 3 2 2)(4 7 5 6)))
(assert (=~i "a(x*)b" "-axxb-ab-") '((1 5 2 4)(6 8 7 7)))

;; test for hash-tables
(let ((h1 (make-hash-table)))
  (setf (gethash 'width h1) 600)
  (setf (gethash 'height h1) 300)
  (assert (gethash 'width h1) 600)
  (assert (gethash 'height h1) 300)
  (assert (hash-table-count h1) 2)
  (remhash 'width h1)
  (assert (gethash 'width h1) nil)
  (clrhash h1)
  (assert (hash-table-count h1) 0))

;; test for (format)
(assert (format nil "~d" 123) "123")
(assert (format nil "~x" 123) "7B")
(assert (format nil "~o" 123) "173")
(assert (format nil "~b" 123) "1111011")
(assert (format nil "~f" 12.3) "12.3")
(assert (format nil "~e" 12.3) "1.23e+01")
(assert (format nil "~g" 12.3) "12.3")
(assert (format nil "~a" "ABC") "ABC")
(assert (format nil "~s" "ABC") "\"ABC\"")
(assert (format nil "[~5d]" 123) "[  123]")
(assert (format nil "[~5a]" "ABC") "[ABC  ]")

;;; test for (format-integer)
(assert
  (let ((s (create-string-output-stream)))
    (format-integer s 123 10)
    (get-output-stream-string s)
    ) "123")

;;; test for (format-char)
(assert
  (let ((s (create-string-output-stream)))
    (format-char s #\A)
    (format-char s #\B)
    (get-output-stream-string s)
    ) "AB")

;;; test for (format-object ... t)
(assert
  (let ((s (create-string-output-stream)))
    (format-object s "ahaha" t)
    (get-output-stream-string s)
    ) "\"ahaha\"")

;;; test for (format-object ... nil)
(assert
  (let ((s (create-string-output-stream)))
    (format-object s "ahaha" nil)
    (get-output-stream-string s)
    ) "ahaha")

;;; test for (format-float)
(assert
  (let ((s (create-string-output-stream)))
    (format-float s 0.3)
    (get-output-stream-string s)
    ) "0.3")

;;; test for (string<=>)
(assert (string< "a" "b") t)
(assert (string> "a" "b") nil)
(assert (string<= "a" "b") t)
(assert (string>= "a" "b") nil)
(assert (string= "a" "b") nil)
(assert (string/= "a" "b") t)

;;; test for (string-index)
(assert (string-index "foo" "foobar") 0)
(assert (string-index "bar" "foobar") 3)
(assert (string-index "FOO" "foobar") nil)
(assert (string-index "foo" "foobar" 1) nil)
(assert (string-index "bar" "foobar" 1) 3)
(assert (string-index "foo" "") nil)
(assert (string-index "" "foo") 0)

;;; test for (create-string)
(assert (create-string 1 #\A) "A")
(assert (create-string 2 #\B) "BB")

;;; test for operator
(assert (+ 1 2) 3)
(assert (+ 1 2 3) 6)
(assert (- 10 9) 1)
(assert (- 10 1 2) 7)
(assert (* 1 2) 2)
(assert (* 1 2 3) 6)
(assert (/ 6 2) 3)
(assert (+ "1" "2") "12")
(assert (> 2 1.0) t)
(assert (> 2.0 3) nil)
(assert (< 2.0 3) t)
(assert (< 2 1.0) nil)
(assert (<= 2.0 3) t)
(assert (<= 3 3) t)
(assert (<= 4 3) nil)
(assert (>= 2.0 3) nil)
(assert (>= 3 3) t)
(assert (>= 4 3) t)
(assert (> "a" "b") nil)
(assert (< "a" "b" "c") t)
(assert (< 1 2 3) t)
(assert (< 1 2 1) nil)
(assert (>= 3 2 2) t)
(assert (>= 2 2 2) t)
(assert (> 3 2 1) t)
(assert (= 1 1) t)
(assert (= 1.0 1) t)
(assert (= 1 1.0) t)
(assert (= 1.0 1.0) t)
(assert (= 1.0 1.0 1.0) t)
(assert (= 1 2) nil)
(assert (= 1 2.0) nil)
(assert (= 1.0 2) nil)
(assert (= 1 2.0) nil)
(assert (= "ABC" "abc") t)
(assert (= "ABC" "abcd") nil)
(assert (equalp "DEF" "defg") nil)
(assert (equalp "GHQ" "ghq") t)
(assert (equalp (cons 1 (cons 2 nil)) '(1 2)) t)
(assert (equalp (cons 1 2) '(1)) nil)
(assert (and 1) 1)
(assert (and 1 2) 2)
(assert (and 1 2 3) 3)
(assert (and 1 nil 3) nil)
(assert (or 1) 1)
(assert (or 1 2) 1)
(assert (or 1 2 3) 1)
(assert (or 1 nil 3) 1)
(assert (or nil 3) 3)
(assert (1+ 10) 11)
(assert (1- 10) 9)

;;; test for (mod)
(assert (mod -5 3) 1)
(assert (mod 5 -3) -1)
(assert (rem -5 3) -2)
(assert (rem 5 -3) 2)

;;; test for (truncate)
(assert (truncate 1.6) 1)
(assert (truncate -1.6) -1)

;;; test for (ceiling)
(assert (ceiling 1.6) 2)
(assert (ceiling -1.6) -1)

;;; test for (floor)
(assert (floor 1.6) 1)
(assert (floor -1.6) -2)

;;; test for (round)
(assert (round 1.6) 2)
(assert (round -1.6) -2)

;;; test for (length)
(assert (length (list 1 2 3 4)) 4)
(assert (length '(list 1 2 3)) 4)
(assert (length "12345") 5)

;;; test for mapcar
(assert (mapcar (function +) '(1 2 3) '(4 5 6))
        '(5 7 9))
(assert (mapcar #'+ '(1 2 3) '(4 5 6))
        '(5 7 9))
(assert (mapcar '+ '(1 2 3) '(4 5 6))
        '(5 7 9))
(assert (mapcar (lambda (a b) (+ a b)) '(1 2 3) '(4 5 6))
        '(5 7 9))
(assert (mapcar #'(lambda (a b) (+ a b)) '(1 2 3) '(4 5 6))
        '(5 7 9))
(assert (mapcar #'car '((1 a) (2 b) (3 c)))
        '(1 2 3))
(assert (mapcar #'cons '(a b c) '(1 2 3))
        '((a . 1) (b . 2) (c . 3)))

;;; test for mapc
(assert (let ((buffer (create-string-output-stream)) result)
          (setq result (mapc (lambda (c) (format-char buffer (1+ c))) "ABC"))
          (list result (get-output-stream-string buffer))
          )
        '("ABC" "BCD"))

;;; test for mapcan
(assert (mapcan (lambda (x) (if (> x 0) (list x))) '(-3 4 0 5 -2 7))
        '(4 5 7))

;;; test for maplist
(assert (maplist #'append '(1 2 3 4) '(1 2) '(1 2 3))
        '((1 2 3 4 1 2 1 2 3) (2 3 4 2 2 3)))

;;; test for mapL
(assert (let ((k 0))
          (mapl
            (lambda (x)
              (setq k (+ k (if (member (car x) (cdr x)) 0 1)))
              )
            '(a b a c d b c)
            )
          k)
        4)

;;; test for mapcon
(assert (mapcon
          (lambda (x)
            (if (member (car x) (cdr x)) (list (car x)))
            )
          '(a b a c d b c b c)
          )
        '(a b c b c))

;;; test for reverse
(assert (reverse '(1 2 3 4))
        '(4 3 2 1))

;;; test for nreverse
(assert (nreverse '(1 2 3 4))
        '(4 3 2 1))

;;; test for subseq
(assert (subseq "12345" 2 4) "34")

(assert (subseq '(1 2 3 4 5) 2 4)
        '(3 4))

;;; test for (setf (subseq...))
(assert (let ((m "12345"))
          (setf (subseq m 2 4) "xx")
          m)
        "12xx5")

(assert (let ((m (list 1 2 3 4 5)))
          (setf (subseq m 2 4) (list 0 0))
          m)
        '(1 2 0 0 5))

;;; test for elt
(assert (elt '(a b c) 2)
        'c)
(assert (elt #('a 'b 'c) 1)
        'b)
(assert (elt "abc" 0)
        #\a)

;;; test for if
(assert (if t 1 2) 1)
(assert (if nil 1 2) 2)
(assert (if t 3) 3)
(assert (if nil 3) nil)
(assert (let (aa)
          (cond
            ((= (setq aa (string-append "a" "a")) "ab") "A")
            ((= aa "aa") "B")
            (T "fail")
            )
          ) "B")

;;; test for (case)
(assert (case 2
          ((1) "a")
          ((2) "b")
          ((3) "c")
          )
        "b")

(assert (case 4
          ((1 2) "A")
          ((3 4) "B")
          ((5 6) "C")
          )
        "B")

(assert (case 7
          ((1 2) "A")
          ((3 4) "B")
          (t "C")
          )
        "C")

;;; test for (with-handler)
(assert (catch 'hoge
               (with-handler
                 (lambda (c)
                   (if (eql c *err-variable-unbound*)
                     (throw 'hoge "OK")))
                 (not-exist-func)
                 "NG"
                 )
               )
        "OK")

;;; test for (catch) (throw)
(labels ((foo (x)
              (catch 'block-sum (bar x))
              )
         (bar (x)
              (let (sum L)
                (for ((L x (cdr L))
                      (sum 0 (+ sum (car L))))
                     ((null L) sum)
                     (cond
                       ((not (numberp (car L))) (throw 'block-sum 0))
                       )
                     )
                )
              )
         )
  (assert 10 (foo '(1 2 3 4)))
  (assert 0 (foo '(1 2 nil 4)))
  )

;;; test for (unwind-protect) and (throw)
(labels ((foo ()
              (throw 'hogehoge 10)
              ))
  (let ((x 0))
    (assert
      (catch 'hogehoge
             (unwind-protect
               (foo)
               (setq x 1)
               )
             )
      10
      )
    (assert x 1)
    )
  )

;;; test for (atom)
(assert (atom 1) t)
(assert (atom '(1 2)) nil)

;;; parse-number
(assert (parse-number "1") 1)
(assert (parse-number "1.1") 1.1)

;;; create-string-input-stream
(assert
  (let*
    ((lf (create-string 1 #\newline))
     (fd (create-string-input-stream (string-append "1" lf "2" lf "3"))))
    (read-line fd)
    (read-line fd)
    )
  "2")

;;; test any types
(assert (integerp 1) t)
(assert (integerp "") nil)
(assert (floatp 1.1) t)
(assert (floatp 1) nil)
(assert (symbolp 'a) t)
(assert (symbolp 1) nil)
(assert (stringp "1") t)
(assert (stringp 1) nil)
(assert (consp '(1) ) t)
(assert (consp 1) nil)
(assert (functionp consp) t)

;;; test (evenp)
(assert (evenp 0) t)
(assert (evenp 1) nil)

;;; test (oddp)
(assert (oddp 1) t)
(assert (oddp 0) nil)

;;; test (null)
(assert (null 1) nil)
(assert (null "") nil)
(assert (null nil) t)

;;; test (minusp)
(assert (minusp 1) nil)
(assert (minusp 1.0) nil)
(assert (minusp -1) t)
(assert (minusp -1.0) t)
(assert (minusp "") nil)

;;; test (plusp)
(assert (plusp 1) t)
(assert (plusp 1.0) t)
(assert (plusp -1) nil)
(assert (plusp -1.0) nil)
(assert (plusp "") nil)

;;; test (numberp)
(assert (numberp 1) t)
(assert (numberp 1.0) t)
(assert (numberp "") nil)

;;; test (zerop)
(assert (zerop 0) t)
(assert (zerop 0.0) t)
(assert (zerop 1) nil)
(assert (zerop 0.1) nil)
(assert (zerop "") nil)

;;; test for (/=)
(assert (not (/= 1 1)) t)
(assert (not (= 1 1)) nil)
(assert (/= 1 2) t)
(assert (/= 1 1) nil)

;;; test for (car)
(assert (car '(1 2)) 1)
(assert (car '(1 . 2)) 1)
;;; test for (cdr)
(assert (cdr '(1 . 2)) 2)
(assert (cdr '(1 2)) '(2))
;;; test for (list)
(assert (list 1 2 3 4) '(1 2 3 4))
;;; test for (cons)
(assert (cons 1 2) '(1 . 2))

;;; test for (listp)
(assert (listp ()) t)
(assert (listp 1) nil)
(assert (listp '(1 2 3)) t)

;;; test for (assoc)
(assert (let ((collection '((a . 1) (b . 2) (c . 3))))
          (assoc 'a collection))
        '(a . 1))

;;; test for (append)
(assert (append '(1 2) '(3 4))
        '(1 2 3 4))
(assert (append '(1 2) '(3 4) '(5 6))
        '(1 2 3 4 5 6))
(assert (append '() '(1 2) '(3 4))
        '(1 2 3 4))

;append do not destruct original not last list.
(assert (let ((x '(1 2 3)))
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
  (assert part1 '(1 2 3))
  (assert part2 '(4 5 6))
  (assert part3 '(7 "ccc" 9)))

(assert (append '() '(1)) '(1))

;;; test for (last)
(assert (last '(1 2 3 4)) 4)
(assert (last '()) nil)

;;; test for (set-car)
(assert (let ((c '("A" . "D"))) (set-car "X" c) c)
        '("X" . "D"))

;;; test for (set-cdr)
(assert (let ((c '("A" . "D"))) (set-cdr "X" c) c)
        '("A" . "X"))

;;; test create-string-output-stream
(assert (let ((str (create-string-output-stream)))
          (format str "hello")
          (format str "world")
          (get-output-stream-string str))
        "helloworld")

;;; test for (read)
(let ((r (create-string-input-stream "1 \"ahaha\" 3")))
  (assert (read r nil "EOF") 1)
  (assert (read r nil "EOF") "ahaha")
  (assert (read r nil "EOF") 3)
  (assert (read r nil "EOF") "EOF"))

;;; test for (read-line)
(let*
  ((lf (create-string 1 #\newline))
   (s (string-append "1" lf "2" lf "3"))
   (r (create-string-input-stream s)))
  (assert (read-line r nil "EOF") "1")
  (assert (read-line r nil "EOF") "2")
  (assert (read-line r nil "EOF") "3")
  (assert (read-line r nil "EOF") "EOF")
  )

;;; test for (with-open-input-file)
(assert (with-open-input-file (fd "LICENSE")
                              (read-line fd))
        "MIT License")

;;; test for (probe-file)
(assert (probe-file ".") t)
(assert (probe-file "notexist.lsp") nil)

;;; test for (let)
(assert (let* ((x 2)(y x)) y) 2)
(assert (let ((x 0)) (let ((x 2)(y x)) y)) 0)

;;; test for (defglobal)
(assert (defglobal a "ahaha") 'a)
(assert (progn (defglobal a "ahaha")(defglobal a "ihihi") a) "ihihi")

;;; test for (setf)
(assert (let (x)
          (setf (car (setq x (cons 1 2))) 3)
          x)
        '(3 . 2))

(assert (progn (defglobal x (cons 1 2))
               (setf (cdr x) 3)
               x)
        '(1 . 3))
(assert (let ((m (list (cons 1 "A") (cons 2 "B") (cons 3 "C"))))
          (setf (cdr (assoc 1 m)) "X")
          m)
        '((1 . "X")
          (2 . "B")
          (3 . "C")))

(assert (let ((m '((1 . "A") (2 . "B") (3 . "C"))) pair )
          (if (setq pair (assoc 1 m))
            (setf (cdr pair) "X")
            )
          m)
        '((1 . "X")
          (2 . "B")
          (3 . "C")))

;;; test for dynamic
(assert (progn (defdynamic *color* 'red)
               (defun what-color () (dynamic *color*))
               (what-color))
        'red)

(assert (progn (defdynamic hoge 1)
               (setf (dynamic hoge) 3)
               (dynamic hoge))
        3)

(assert (progn (defdynamic *color* 'red)
               (defun what-color () (dynamic *color*))
               (dynamic-let ((*color* 'green)) (what-color)))
        'green)

;;; test macro

(assert
  (progn
    (defmacro dbl (x) (list '* x 2))
    (dbl 3)
    )
  6)

(assert
  (progn
    (defmacro dbl  (x) (list '+ x x))
    (defmacro incf (y) (list 'setq y (list '+ y 1)))
    (let ((a1 2))
      (dbl (incf a1)))
    )
  7)

(assert (list ''foo) (list (list 'quote 'foo)))

(assert
  (progn
    (defmacro dolist1 (pair &rest commands)
      (let ((key (car pair))
            (values (car (cdr pair))))
        `(mapc (lambda (,key) ,@commands) ,values)
        )
      )
    (let ((result nil))
      (dolist1 (x '(1 2 3)) (setq result (cons x result)))
      result
      )
    )
  '(3 2 1))

(assert (progn
          (defmacro dolist2 (pair :rest commands)
            (let ((key (car pair))
                  (values (car (cdr pair))))
              `(mapc (lambda (,key) ,@commands) ,values)
              )
            )
          (let ((result nil))
            (dolist2 (x '(1 2 3)) (setq result (cons x result)))
            result
            ))
        '(3 2 1))

;;; test for cond
(assert
  (cond (nil 1) (t 2))
  2)

(assert
  (cond ((equal 1 1) "a") ((equal 1 2) "b"))
  "a")

;;; test for progn
(assert (progn 1) 1)
(assert (progn 1 2) 2)

;;; test for quote
(assert (quote (1 . 2))
				(cons 1 2))
(assert (equal (list 1 (+ 1 1) (+ 1 2)) '(1 2 3))
				t)

;;; test for eq/eql/equal/equalp
(assert (eq 1 1)
				t)
(assert (eq 1 2)
				nil)
(assert (eq (cons 1 2) (cons 1 2))
				nil)
(assert (let ((a (cons 1 2))) (eq a a))
				t)
(assert (eql 1 1)
				t)
(assert (eql 1 2)
				nil)
(assert (eql (cons 1 2) (cons 1 2))
				nil)
(assert (equal (cons 1 2) (cons 1 2))
				t)
(assert (let ((a (cons 1 2))) (eql a a))
				t)
(assert (eql 1 1)
				t)
(assert (eql 1 1.0)
				nil)
(assert (equal 1 1.0)
				nil)
(assert (equal "A" "A")
				t)
(assert (equal "a" "A")
				nil)
(assert (equalp "a" "A")
				t)
(assert (equalp 1 1.0)
				t)
