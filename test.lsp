(defmacro test (source expect)
  (let ((result (gensym)))
    `(let ((,result ,source))
       (if (not (equal ,result ,expect))
         (progn
           (format (error-output) "Failed: ~s~%" (quote ,source))
           (format (error-output) "  expect: ~s but ~s~%" ,expect ,result)
           (abort))))))

;;; test for create-array ;;;
(let ((A (create-array '(3 3) 0)))
  (test (aref A 1 1) 0)
  (set-aref 2 A 1 1)
  (test (aref A 1 1) 2)
  ;(format t "~s~%" (aref A 1))
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
  (test step 1))

; backword test
(let ((step 0))
  (test
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
  (test x 2)
  (test y 1))

;;; test for (when) ;;;
(test (when t 1 2 3) 3)
(test (when nil 1 2 3) nil)

;;; test for (unless) ;;;
(test (unless t 1 2 3) nil)
(test (unless nil 1 2 3) 3)

;;; test for (prog1) ;;;
(test (prog1 1 2 3 4) 1)
(test (prog2 1 2 3 4) 2)

;;; test for cdr ;;;
(test (cadr '(a b c d)) 'b)

;;; test for (caddr) ;;;
(test (caddr '(a b c d)) 'c)

;;; test for (cadddr) ;;;
(test (cadddr '(a b c d)) 'd)

;;; test for (cddr) ;;;
(test (cddr '(a b c d e)) '(c d e))

;;; test for (cdddr) ;;;
(test (cdddr '(a b c d e)) '(d e))

;;; test for (first) ;;;
(test (first '(a b c d)) 'a)

;;; test for (second) ;;;
(test (second '(a b c d)) 'b)

;;; test for (third) ;;;
(test (third '(a b c d)) 'c)

;;; test for (rest) ;;;
(test (rest '(a b c d)) '(b c d))

;;; test for (nth) ;;;
(test (nth 2 '(a b c d)) 'c)

;;; test for (setf cadr)
(test (let ((L '(a b c d))) (setf (cadr L) 'x) L) '(a x c d))

;;; test for (setf caddr)
(test (let ((L '(a b c d))) (setf (caddr L) 'x) L) '(a b x d))

;;; test for (setf cadddr)
(test (let ((L '(a b c d))) (setf (cadddr L) 'x) L) '(a b c x))

;;; test for (setf first)
(test (let ((L '(a b c d))) (setf (first L) 'x) L) '(x b c d))

;;; test for (setf second)
(test (let ((L '(a b c d))) (setf (second L) 'x) L) '(a x c d))

;;; test for (setf third)
(test (let ((L '(a b c d))) (setf (third L) 'x) L) '(a b x d))

;;; test for (setf nth)
(test (let ((L '(a b c d))) (setf (nth 3 L) 'x) L) '(a b c x))

;;; test for (setf cddr)
(test (let ((L '(a b c d))) (setf (cddr L) 'x) L) '(a b . x))

;;; test for (setf cdddr)
(test (let ((L '(a b c d))) (setf (cdddr L) 'x) L) '(a b c . x))

;; test for hash-tables
(let ((h1 (make-hash-table)))
  (setf (gethash 'width h1) 600)
  (setf (gethash 'height h1) 300)
  (test (gethash 'width h1) 600)
  (test (gethash 'height h1) 300)
  (test (hash-table-count h1) 2)
  (remhash 'width h1)
  (test (gethash 'width h1) nil)
  (clrhash h1)
  (test (hash-table-count h1) 0))

;;; test for (string-index)
(test (string-index "foo" "foobar") 0)
(test (string-index "bar" "foobar") 3)
(test (string-index "FOO" "foobar") nil)
(test (string-index "foo" "foobar" 1) nil)
(test (string-index "bar" "foobar" 1) 3)
(test (string-index "foo" "") nil)
(test (string-index "" "foo") 0)

;;; test for (create-string)
(test (create-string 1 #\A) "A")
(test (create-string 2 #\B) "BB")

;;; test for operator
(test (+ 1 2) 3)
(test (+ 1 2 3) 6)
(test (- 10 9) 1)
(test (- 10 1 2) 7)
(test (* 1 2) 2)
(test (* 1 2 3) 6)
(test (div 6 2) 3)
(test (+ "1" "2") "12")
(test (> 2 1.0) t)
(test (> 2.0 3) nil)
(test (< 2.0 3) t)
(test (< 2 1.0) nil)
(test (<= 2.0 3) t)
(test (<= 3 3) t)
(test (<= 4 3) nil)
(test (>= 2.0 3) nil)
(test (>= 3 3) t)
(test (>= 4 3) t)
(test (> "a" "b") nil)
(test (< "a" "b" "c") t)
(test (< 1 2 3) t)
(test (< 1 2 1) nil)
(test (>= 3 2 2) t)
(test (>= 2 2 2) t)
(test (> 3 2 1) t)
(test (= 1 1) t)
(test (= 1.0 1) t)
(test (= 1 1.0) t)
(test (= 1.0 1.0) t)
(test (= 1.0 1.0 1.0) t)
(test (= 1 2) nil)
(test (= 1 2.0) nil)
(test (= 1.0 2) nil)
(test (= 1 2.0) nil)
(test (= "ABC" "abc") t)
(test (= "ABC" "abcd") nil)
(test (equalp "DEF" "defg") nil)
(test (equalp "GHQ" "ghq") t)
(test (equalp (cons 1 (cons 2 nil)) '(1 2)) t)
(test (equalp (cons 1 2) '(1)) nil)
(test (and 1) 1)
(test (and 1 2) 2)
(test (and 1 2 3) 3)
(test (and 1 nil 3) nil)
(test (or 1) 1)
(test (or 1 2) 1)
(test (or 1 2 3) 1)
(test (or 1 nil 3) 1)
(test (or nil 3) 3)
(test (1+ 10) 11)
(test (1- 10) 9)

;;; test for (mod)
(test (mod -5 3) 1)
(test (mod 5 -3) -1)
(test (rem -5 3) -2)
(test (rem 5 -3) 2)

;;; test for (truncate)
(test (truncate 1.6) 1)
(test (truncate -1.6) -1)

;;; test for (ceiling)
(test (ceiling 1.6) 2)
(test (ceiling -1.6) -1)

;;; test for (floor)
(test (floor 1.6) 1)
(test (floor -1.6) -2)

;;; test for (round)
(test (round 1.6) 2)
(test (round -1.6) -2)

;;; test for (length)
(test (length (list 1 2 3 4)) 4)
(test (length '(list 1 2 3)) 4)
(test (length "12345") 5)

;;; test for mapcar
(test (mapcar (function +) '(1 2 3) '(4 5 6))
      '(5 7 9))
(test (mapcar #'+ '(1 2 3) '(4 5 6))
      '(5 7 9))
(test (mapcar (lambda (a b) (+ a b)) '(1 2 3) '(4 5 6))
      '(5 7 9))
(test (mapcar #'car '((1 a) (2 b) (3 c)))
      '(1 2 3))
(test (mapcar #'cons '(a b c) '(1 2 3))
      '((a . 1) (b . 2) (c . 3)))

;;; test for mapc
(test (let ((buffer (create-string-output-stream)) result)
        (setq result (mapc (lambda (c) (format-char buffer (1+ c))) "ABC"))
        (list result (get-output-stream-string buffer))
        )
      '("ABC" "BCD"))

;;; test for mapcan
(test (mapcan (lambda (x) (if (> x 0) (list x))) '(-3 4 0 5 -2 7))
      '(4 5 7))

;;; test for maplist
(test (maplist #'append '(1 2 3 4) '(1 2) '(1 2 3))
      '((1 2 3 4 1 2 1 2 3) (2 3 4 2 2 3)))

;;; test for mapL
(test (let ((k 0))
        (mapl
          (lambda (x)
            (setq k (+ k (if (member (car x) (cdr x)) 0 1)))
            )
          '(a b a c d b c)
          )
        k)
      4)

;;; test for mapcon
(test (mapcon
        (lambda (x)
          (if (member (car x) (cdr x)) (list (car x)))
          )
        '(a b a c d b c b c)
        )
      '(a b c b c))

;;; test for subseq
(test (subseq "12345" 2 4) "34")

(test (subseq '(1 2 3 4 5) 2 4)
      '(3 4))

;;; test for (setf (subseq...))
(test (let ((m "12345"))
        (setf (subseq m 2 4) "xx")
        m)
      "12xx5")

(test (let ((m (list 1 2 3 4 5)))
        (setf (subseq m 2 4) (list 0 0))
        m)
      '(1 2 0 0 5))

;;; test for elt
(test (elt '(a b c) 2)
      'c)
(test (elt #(a b c) 1)
      'b)
(test (elt "abc" 0)
      #\a)

;;; test for if
(test (if t 1 2) 1)
(test (if nil 1 2) 2)
(test (if t 3) 3)
(test (if nil 3) nil)
(test (let (aa)
        (cond
          ((= (setq aa (string-append "a" "a")) "ab") "A")
          ((= aa "aa") "B")
          (T "fail")
          )
        ) "B")

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
  (test 10 (foo '(1 2 3 4)))
  (test 0 (foo '(1 2 nil 4)))
  )

;;; test for (unwind-protect) and (throw)
(labels ((foo ()
              (throw 'hogehoge 10)
              ))
  (let ((x 0))
    (test
      (catch 'hogehoge
             (unwind-protect
               (foo)
               (setq x 1)
               )
             )
      10
      )
    (test x 1)
    )
  )

;;; test for (atom)
(test (atom 1) t)
(test (atom '(1 2)) nil)

;;; parse-number
(test (parse-number "1") 1)
(test (parse-number "1.1") 1.1)

;;; create-string-input-stream
(test
  (let*
    ((lf (create-string 1 #\newline))
     (fd (create-string-input-stream (string-append "1" lf "2" lf "3"))))
    (read-line fd)
    (read-line fd)
    )
  "2")

;;; test any types
(test (integerp 1) t)
(test (integerp "") nil)
(test (floatp 1.1) t)
(test (floatp 1) nil)
(test (symbolp 'a) t)
(test (symbolp 1) nil)
(test (stringp "1") t)
(test (stringp 1) nil)
(test (consp '(1) ) t)
(test (consp 1) nil)
(test (functionp #'consp) t)

;;; test (evenp)
(test (evenp 0) t)
(test (evenp 1) nil)

;;; test (oddp)
(test (oddp 1) t)
(test (oddp 0) nil)

;;; test (null)
(test (null 1) nil)
(test (null "") nil)
(test (null nil) t)

;;; test (minusp)
(test (minusp 1) nil)
(test (minusp 1.0) nil)
(test (minusp -1) t)
(test (minusp -1.0) t)
(test (minusp "") nil)

;;; test (plusp)
(test (plusp 1) t)
(test (plusp 1.0) t)
(test (plusp -1) nil)
(test (plusp -1.0) nil)
(test (plusp "") nil)

;;; test (numberp)
(test (numberp 1) t)
(test (numberp 1.0) t)
(test (numberp "") nil)

;;; test (zerop)
(test (zerop 0) t)
(test (zerop 0.0) t)
(test (zerop 1) nil)
(test (zerop 0.1) nil)
(test (zerop "") nil)

;;; test for (/=)
(test (not (/= 1 1)) t)
(test (not (= 1 1)) nil)
(test (/= 1 2) t)
(test (/= 1 1) nil)

;;; test for (car)
(test (car '(1 2)) 1)
(test (car '(1 . 2)) 1)
;;; test for (cdr)
(test (cdr '(1 . 2)) 2)
(test (cdr '(1 2)) '(2))
;;; test for (cons)
(test (cons 1 2) '(1 . 2))

;;; test for (assoc)
(test (let ((collection '((a . 1) (b . 2) (c . 3))))
        (assoc 'a collection))
      '(a . 1))

;;; test for (last)
(test (last '(1 2 3 4)) 4)
(test (last '()) nil)

;;; test for (set-car)
(test (let ((c '("A" . "D"))) (set-car "X" c) c)
      '("X" . "D"))

;;; test for (set-cdr)
(test (let ((c '("A" . "D"))) (set-cdr "X" c) c)
      '("A" . "X"))

;;; test create-string-output-stream
(test (let ((str (create-string-output-stream)))
        (format str "hello")
        (format str "world")
        (get-output-stream-string str))
      "helloworld")

;;; test for (read)
(let ((r (create-string-input-stream "1 \"ahaha\" 3")))
  (test (read r nil "EOF") 1)
  (test (read r nil "EOF") "ahaha")
  (test (read r nil "EOF") 3)
  (test (read r nil "EOF") "EOF"))

;;; test for (read-line)
(let*
  ((lf (create-string 1 #\newline))
   (s (string-append "1" lf "2" lf "3"))
   (r (create-string-input-stream s)))
  (test (read-line r nil "EOF") "1")
  (test (read-line r nil "EOF") "2")
  (test (read-line r nil "EOF") "3")
  (test (read-line r nil "EOF") "EOF")
  )

;;; test for (with-open-input-file)
(test (with-open-input-file (fd "LICENSE")
                            (read-line fd))
      "MIT License")

;;; test for (probe-file)
(test (probe-file ".") t)
(test (probe-file "notexist.lsp") nil)

;;; test for (let)
(test (let* ((x 2)(y x)) y) 2)
(test (let ((x 0)) (let ((x 2)(y x)) y)) 0)

;;; test for (defglobal)
(test (defglobal a "ahaha") 'a)
(test (progn (defglobal a "ahaha")(defglobal a "ihihi") a) "ihihi")

;;; test for (setf)
(test (let (x)
        (setf (car (setq x (cons 1 2))) 3)
        x)
      '(3 . 2))

(test (progn (defglobal x (cons 1 2))
             (setf (cdr x) 3)
             x)
      '(1 . 3))
(test (let ((m (list (cons 1 "A") (cons 2 "B") (cons 3 "C"))))
        (setf (cdr (assoc 1 m)) "X")
        m)
      '((1 . "X")
        (2 . "B")
        (3 . "C")))

(test (let ((m '((1 . "A") (2 . "B") (3 . "C"))) pair )
        (if (setq pair (assoc 1 m))
          (setf (cdr pair) "X")
          )
        m)
      '((1 . "X")
        (2 . "B")
        (3 . "C")))

;;; test for dynamic
(test (progn (defdynamic *color* 'red)
             (defun what-color () (dynamic *color*))
             (what-color))
      'red)

(test (progn (defdynamic hoge 1)
             (setf (dynamic hoge) 3)
             (dynamic hoge))
      3)

(test (progn (defdynamic *color* 'red)
             (defun what-color () (dynamic *color*))
             (dynamic-let ((*color* 'green)) (what-color)))
      'green)

;;; test macro

(test
  (progn
    (defmacro dbl (x) (list '* x 2))
    (dbl 3)
    )
  6)

(test
  (progn
    (defmacro dbl  (x) (list '+ x x))
    (defmacro incf (y) (list 'setq y (list '+ y 1)))
    (let ((a1 2))
      (dbl (incf a1)))
    )
  7)

(test (list ''foo) (list (list 'quote 'foo)))

(test
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

(test (progn
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
(test
  (cond (nil 1) (t 2))
  2)

(test
  (cond ((equal 1 1) "a") ((equal 1 2) "b"))
  "a")

;;; test for progn
(test (progn 1) 1)
(test (progn 1 2) 2)

;;; test for quote
(test (quote (1 . 2))
      (cons 1 2))
(test (equal (list 1 (+ 1 1) (+ 1 2)) '(1 2 3))
      t)

;;; test for eq/eql/equal/equalp
(test (eq 1 1)
      t)
(test (eq 1 2)
      nil)
(test (eq (cons 1 2) (cons 1 2))
      nil)
(test (let ((a (cons 1 2))) (eq a a))
      t)
(test (eql 1 1)
      t)
(test (eql 1 2)
      nil)
(test (eql (cons 1 2) (cons 1 2))
      nil)
(test (equal (cons 1 2) (cons 1 2))
      t)
(test (let ((a (cons 1 2))) (eql a a))
      t)
(test (eql 1 1)
      t)
(test (eql 1 1.0)
      nil)
(test (equal 1 1.0)
      nil)
(test (equal "A" "A")
      t)
(test (equal "a" "A")
      nil)
(test (equalp "a" "A")
      t)
(test (equalp 1 1.0)
      t)

;;; test for (flet)
(test (flet ((f (x) (+ x 3)))
        (flet ((f (x) (+ x (f x))))
          (f 7)
          )
        )
      17)

;;; test for (labels)

(test (labels
        ((evenp (n)
                (if (= n 0)
                  t
                  (oddp (- n 1))))
         (oddp (n)
               (if (= n 0)
                 nil
                 (evenp (- n 1)))))
        (evenp 88))
      t)

;;; test for &rest
(test
  (progn
    (defun cat (left &rest args)
      (apply #'string-append  left args)
      )
    (cat "1" "2" "3" "4" "5"))
  "12345")

;;; test for (apply)
(test (apply #'+ '(1 2 3))
      6)
(test (apply #'+ 4 5 6 '(1 2 3))
      21)

;;; test for (funcall)
(test (let ((f (lambda (a b) (+ a b))))
        (funcall f 1 2))
      3)

;;; test for lambda
(test (progn
        (defun f (a b)
          (+ a b))
        (f 1 2))
      3)

(test (progn
        (defun f1 (a b)
          (+ a b))
        (f1 1.0 2.0))
      3.0)

(test (let ((f2 (lambda (a b) (+ a b))))
        (funcall f2 4 5))
      9)

(test (let (a)
        (setq a 0)
        (defun dummy (a b) (+ a b))
        (dummy 7 8)
        a)
      0)

(test (progn (let ((x 1))
               (defun f ()
                 (list x)))
             (let ((x 2))
               (f)))
      (list 1))

(test (let (c)(setq c "a") c)
      "a")

(test (progn
        (defglobal c "a")
        (defun f (a)
          (let ((c "b"))
            (+ a 1)
            )
          )
        (list (f 4) c))
      (list 5 "a"))

(test (progn
        (defglobal c "a")
        (defun f (a / c)
          (setq c "b")
          (+ a 1)
          )
        (list (f 4) c)
        )
      (list 5 "a")
      )

(test (let ((a 0)) (if t (setq a 1) (setq a 2)) a)
      1)
(test (let ((x "1")) (if nil (setq x "2") (setq x "3")) x)
      "3")

(dolist (testcode (wildcard "test/*.lsp"))
  (format t "test ~s~%" testcode)
  (load testcode))
