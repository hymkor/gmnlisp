;;; test for create-array ;;;
(let ((A (create-array '(3 3) 0)))
  (assert-eq (aref A 1 1) 0)
  (set-aref 2 A 1 1)
  (assert-eq (aref A 1 1) 2)
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
  (assert-eq step 1))

; backword test
(let ((step 0))
  (assert-eq
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
  (assert-eq x 2)
  (assert-eq y 1))

;;; test for (when) ;;;
(assert-eq (when t 1 2 3) 3)
(assert-eq (when nil 1 2 3) nil)

;;; test for (unless) ;;;
(assert-eq (unless t 1 2 3) nil)
(assert-eq (unless nil 1 2 3) 3)

;;; test for (prog1) ;;;
(assert-eq (prog1 1 2 3 4) 1)
(assert-eq (prog2 1 2 3 4) 2)

;;; test for cdr ;;;
(assert-eq (cadr '(a b c d)) 'b)

;;; test for (caddr) ;;;
(assert-eq (caddr '(a b c d)) 'c)

;;; test for (cadddr) ;;;
(assert-eq (cadddr '(a b c d)) 'd)

;;; test for (cddr) ;;;
(assert-eq (cddr '(a b c d e)) '(c d e))

;;; test for (cdddr) ;;;
(assert-eq (cdddr '(a b c d e)) '(d e))

;;; test for (first) ;;;
(assert-eq (first '(a b c d)) 'a)

;;; test for (second) ;;;
(assert-eq (second '(a b c d)) 'b)

;;; test for (third) ;;;
(assert-eq (third '(a b c d)) 'c)

;;; test for (rest) ;;;
(assert-eq (rest '(a b c d)) '(b c d))

;;; test for (nth) ;;;
(assert-eq (nth 2 '(a b c d)) 'c)

;;; test for (setf cadr)
(assert-eq (let ((L '(a b c d))) (setf (cadr L) 'x) L) '(a x c d))

;;; test for (setf caddr)
(assert-eq (let ((L '(a b c d))) (setf (caddr L) 'x) L) '(a b x d))

;;; test for (setf cadddr)
(assert-eq (let ((L '(a b c d))) (setf (cadddr L) 'x) L) '(a b c x))

;;; test for (setf first)
(assert-eq (let ((L '(a b c d))) (setf (first L) 'x) L) '(x b c d))

;;; test for (setf second)
(assert-eq (let ((L '(a b c d))) (setf (second L) 'x) L) '(a x c d))

;;; test for (setf third)
(assert-eq (let ((L '(a b c d))) (setf (third L) 'x) L) '(a b x d))

;;; test for (setf nth)
(assert-eq (let ((L '(a b c d))) (setf (nth 3 L) 'x) L) '(a b c x))

;;; test for (setf cddr)
(assert-eq (let ((L '(a b c d))) (setf (cddr L) 'x) L) '(a b . x))

;;; test for (setf cdddr)
(assert-eq (let ((L '(a b c d))) (setf (cdddr L) 'x) L) '(a b c . x))

;; test for hash-tables
(let ((h1 (make-hash-table)))
  (setf (gethash 'width h1) 600)
  (setf (gethash 'height h1) 300)
  (assert-eq (gethash 'width h1) 600)
  (assert-eq (gethash 'height h1) 300)
  (assert-eq (hash-table-count h1) 2)
  (remhash 'width h1)
  (assert-eq (gethash 'width h1) nil)
  (clrhash h1)
  (assert-eq (hash-table-count h1) 0))

;;; test for (string-index)
(assert-eq (string-index "foo" "foobar") 0)
(assert-eq (string-index "bar" "foobar") 3)
(assert-eq (string-index "FOO" "foobar") nil)
(assert-eq (string-index "foo" "foobar" 1) nil)
(assert-eq (string-index "bar" "foobar" 1) 3)
(assert-eq (string-index "foo" "") nil)
(assert-eq (string-index "" "foo") 0)

;;; test for (create-string)
(assert-eq (create-string 1 #\A) "A")
(assert-eq (create-string 2 #\B) "BB")

;;; test for operator
(assert-eq (+ 1 2) 3)
(assert-eq (+ 1 2 3) 6)
(assert-eq (- 10 9) 1)
(assert-eq (- 10 1 2) 7)
(assert-eq (* 1 2) 2)
(assert-eq (* 1 2 3) 6)
(assert-eq (div 6 2) 3)
(assert-eq (+ "1" "2") "12")
(assert-eq (> 2 1.0) t)
(assert-eq (> 2.0 3) nil)
(assert-eq (< 2.0 3) t)
(assert-eq (< 2 1.0) nil)
(assert-eq (<= 2.0 3) t)
(assert-eq (<= 3 3) t)
(assert-eq (<= 4 3) nil)
(assert-eq (>= 2.0 3) nil)
(assert-eq (>= 3 3) t)
(assert-eq (>= 4 3) t)
(assert-eq (> "a" "b") nil)
(assert-eq (< "a" "b" "c") t)
(assert-eq (< 1 2 3) t)
(assert-eq (< 1 2 1) nil)
(assert-eq (>= 3 2 2) t)
(assert-eq (>= 2 2 2) t)
(assert-eq (> 3 2 1) t)
(assert-eq (= 1 1) t)
(assert-eq (= 1.0 1) t)
(assert-eq (= 1 1.0) t)
(assert-eq (= 1.0 1.0) t)
; (assert-eq (= 1.0 1.0 1.0) t)
(assert-eq (= 1 2) nil)
(assert-eq (= 1 2.0) nil)
(assert-eq (= 1.0 2) nil)
(assert-eq (= 1 2.0) nil)
(assert-eq (= "ABC" "abc") t)
(assert-eq (= "ABC" "abcd") nil)
(assert-eq (equalp "DEF" "defg") nil)
(assert-eq (equalp "GHQ" "ghq") t)
(assert-eq (equalp (cons 1 (cons 2 nil)) '(1 2)) t)
(assert-eq (equalp (cons 1 2) '(1)) nil)
(assert-eq (and 1) 1)
(assert-eq (and 1 2) 2)
(assert-eq (and 1 2 3) 3)
(assert-eq (and 1 nil 3) nil)
(assert-eq (or 1) 1)
(assert-eq (or 1 2) 1)
(assert-eq (or 1 2 3) 1)
(assert-eq (or 1 nil 3) 1)
(assert-eq (or nil 3) 3)
(assert-eq (1+ 10) 11)
(assert-eq (1- 10) 9)

;;; test for (mod)
(assert-eq (mod -5 3) 1)
(assert-eq (mod 5 -3) -1)
(assert-eq (rem -5 3) -2)
(assert-eq (rem 5 -3) 2)

;;; test for (truncate)
(assert-eq (truncate 1.6) 1)
(assert-eq (truncate -1.6) -1)

;;; test for (ceiling)
(assert-eq (ceiling 1.6) 2)
(assert-eq (ceiling -1.6) -1)

;;; test for (floor)
(assert-eq (floor 1.6) 1)
(assert-eq (floor -1.6) -2)

;;; test for (round)
(assert-eq (round 1.6) 2)
(assert-eq (round -1.6) -2)

;;; test for (length)
(assert-eq (length (list 1 2 3 4)) 4)
(assert-eq (length '(list 1 2 3)) 4)
(assert-eq (length "12345") 5)

;;; test for mapcar
(assert-eq (mapcar (function +) '(1 2 3) '(4 5 6))
      '(5 7 9))
(assert-eq (mapcar #'+ '(1 2 3) '(4 5 6))
      '(5 7 9))
(assert-eq (mapcar (lambda (a b) (+ a b)) '(1 2 3) '(4 5 6))
      '(5 7 9))
(assert-eq (mapcar #'car '((1 a) (2 b) (3 c)))
      '(1 2 3))
(assert-eq (mapcar #'cons '(a b c) '(1 2 3))
      '((a . 1) (b . 2) (c . 3)))

;;; test for mapc
(assert-eq (let ((buffer (create-string-output-stream)) result)
        (setq result (mapc (lambda (c) (format-char buffer (1+ c))) "ABC"))
        (list result (get-output-stream-string buffer))
        )
      '("ABC" "BCD"))

;;; test for mapcan
(assert-eq (mapcan (lambda (x) (if (> x 0) (list x))) '(-3 4 0 5 -2 7))
      '(4 5 7))

;;; test for maplist
(assert-eq (maplist #'append '(1 2 3 4) '(1 2) '(1 2 3))
      '((1 2 3 4 1 2 1 2 3) (2 3 4 2 2 3)))

;;; test for mapL
(assert-eq (let ((k 0))
        (mapl
          (lambda (x)
            (setq k (+ k (if (member (car x) (cdr x)) 0 1)))
            )
          '(a b a c d b c)
          )
        k)
      4)

;;; test for mapcon
(assert-eq (mapcon
        (lambda (x)
          (if (member (car x) (cdr x)) (list (car x)))
          )
        '(a b a c d b c b c)
        )
      '(a b c b c))

;;; test for subseq
(assert-eq (subseq "12345" 2 4) "34")

(assert-eq (subseq '(1 2 3 4 5) 2 4)
      '(3 4))

;;; test for (setf (subseq...))
(assert-eq (let ((m "12345"))
        (setf (subseq m 2 4) "xx")
        m)
      "12xx5")

(assert-eq (let ((m (list 1 2 3 4 5)))
        (setf (subseq m 2 4) (list 0 0))
        m)
      '(1 2 0 0 5))

;;; test for elt
(assert-eq (elt '(a b c) 2)
      'c)
(assert-eq (elt #(a b c) 1)
      'b)
(assert-eq (elt "abc" 0)
      #\a)

;;; test for if
(assert-eq (if t 1 2) 1)
(assert-eq (if nil 1 2) 2)
(assert-eq (if t 3) 3)
(assert-eq (if nil 3) nil)
(assert-eq (let (aa)
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
  (assert-eq 10 (foo '(1 2 3 4)))
  (assert-eq 0 (foo '(1 2 nil 4)))
  )

;;; test for (unwind-protect) and (throw)
(labels ((foo ()
              (throw 'hogehoge 10)
              ))
  (let ((x 0))
    (assert-eq
      (catch 'hogehoge
             (unwind-protect
               (foo)
               (setq x 1)
               )
             )
      10
      )
    (assert-eq x 1)
    )
  )

;;; test for (atom)
(assert-eq (atom 1) t)
(assert-eq (atom '(1 2)) nil)

;;; parse-number
(assert-eq (parse-number "1") 1)
(assert-eq (parse-number "1.1") 1.1)

;;; create-string-input-stream
(assert-eq
  (let*
    ((lf (create-string 1 #\newline))
     (fd (create-string-input-stream (string-append "1" lf "2" lf "3"))))
    (read-line fd)
    (read-line fd)
    )
  "2")

;;; test any types
(assert-eq (integerp 1) t)
(assert-eq (integerp "") nil)
(assert-eq (floatp 1.1) t)
(assert-eq (floatp 1) nil)
(assert-eq (symbolp 'a) t)
(assert-eq (symbolp 1) nil)
(assert-eq (stringp "1") t)
(assert-eq (stringp 1) nil)
(assert-eq (consp '(1) ) t)
(assert-eq (consp 1) nil)
(assert-eq (functionp #'consp) t)

;;; test (evenp)
(assert-eq (evenp 0) t)
(assert-eq (evenp 1) nil)

;;; test (oddp)
(assert-eq (oddp 1) t)
(assert-eq (oddp 0) nil)

;;; test (null)
(assert-eq (null 1) nil)
(assert-eq (null "") nil)
(assert-eq (null nil) t)

;;; test (minusp)
(assert-eq (minusp 1) nil)
(assert-eq (minusp 1.0) nil)
(assert-eq (minusp -1) t)
(assert-eq (minusp -1.0) t)
(assert-eq (minusp "") nil)

;;; test (plusp)
(assert-eq (plusp 1) t)
(assert-eq (plusp 1.0) t)
(assert-eq (plusp -1) nil)
(assert-eq (plusp -1.0) nil)
(assert-eq (plusp "") nil)

;;; test (numberp)
(assert-eq (numberp 1) t)
(assert-eq (numberp 1.0) t)
(assert-eq (numberp "") nil)

;;; test (zerop)
(assert-eq (zerop 0) t)
(assert-eq (zerop 0.0) t)
(assert-eq (zerop 1) nil)
(assert-eq (zerop 0.1) nil)
(assert-eq (zerop "") nil)

;;; test for (/=)
(assert-eq (not (/= 1 1)) t)
(assert-eq (not (= 1 1)) nil)
(assert-eq (/= 1 2) t)
(assert-eq (/= 1 1) nil)

;;; test for (car)
(assert-eq (car '(1 2)) 1)
(assert-eq (car '(1 . 2)) 1)
;;; test for (cdr)
(assert-eq (cdr '(1 . 2)) 2)
(assert-eq (cdr '(1 2)) '(2))
;;; test for (cons)
(assert-eq (cons 1 2) '(1 . 2))

;;; test for (assoc)
(assert-eq (let ((collection '((a . 1) (b . 2) (c . 3))))
        (assoc 'a collection))
      '(a . 1))

;;; test for (last)
(assert-eq (last '(1 2 3 4)) 4)
(assert-eq (last '()) nil)

;;; test for (set-car)
(assert-eq (let ((c '("A" . "D"))) (set-car "X" c) c)
      '("X" . "D"))

;;; test for (set-cdr)
(assert-eq (let ((c '("A" . "D"))) (set-cdr "X" c) c)
      '("A" . "X"))

;;; test create-string-output-stream
(assert-eq (let ((str (create-string-output-stream)))
        (format str "hello")
        (format str "world")
        (get-output-stream-string str))
      "helloworld")

;;; test for (read)
(let ((r (create-string-input-stream "1 \"ahaha\" 3")))
  (assert-eq (read r nil "EOF") 1)
  (assert-eq (read r nil "EOF") "ahaha")
  (assert-eq (read r nil "EOF") 3)
  (assert-eq (read r nil "EOF") "EOF"))

;;; test for (read-line)
(let*
  ((lf (create-string 1 #\newline))
   (s (string-append "1" lf "2" lf "3"))
   (r (create-string-input-stream s)))
  (assert-eq (read-line r nil "EOF") "1")
  (assert-eq (read-line r nil "EOF") "2")
  (assert-eq (read-line r nil "EOF") "3")
  (assert-eq (read-line r nil "EOF") "EOF")
  )

;;; test for (with-open-input-file)
(assert-eq (with-open-input-file (fd "LICENSE")
                            (read-line fd))
      "MIT License")

;;; test for (probe-file)
(assert-eq (probe-file ".") t)
(assert-eq (probe-file "notexist.lsp") nil)

;;; test for (let)
(assert-eq (let* ((x 2)(y x)) y) 2)
(assert-eq (let ((x 0)) (let ((x 2)(y x)) y)) 0)

;;; test for (defglobal)
(assert-eq (defglobal a "ahaha") 'a)
(assert-eq (progn (defglobal a "ahaha")(defglobal a "ihihi") a) "ihihi")

;;; test for (setf)
(assert-eq (let (x)
        (setf (car (setq x (cons 1 2))) 3)
        x)
      '(3 . 2))

(assert-eq (progn (defglobal x (cons 1 2))
             (setf (cdr x) 3)
             x)
      '(1 . 3))
(assert-eq (let ((m (list (cons 1 "A") (cons 2 "B") (cons 3 "C"))))
        (setf (cdr (assoc 1 m)) "X")
        m)
      '((1 . "X")
        (2 . "B")
        (3 . "C")))

(assert-eq (let ((m '((1 . "A") (2 . "B") (3 . "C"))) pair )
        (if (setq pair (assoc 1 m))
          (setf (cdr pair) "X")
          )
        m)
      '((1 . "X")
        (2 . "B")
        (3 . "C")))

;;; test for dynamic
(assert-eq (progn (defdynamic *color* 'red)
             (defun what-color () (dynamic *color*))
             (what-color))
      'red)

(assert-eq (progn (defdynamic hoge 1)
             (setf (dynamic hoge) 3)
             (dynamic hoge))
      3)

(assert-eq (progn (defdynamic *color* 'red)
             (defun what-color () (dynamic *color*))
             (dynamic-let ((*color* 'green)) (what-color)))
      'green)

;;; test macro

(assert-eq
  (progn
    (defmacro dbl (x) (list '* x 2))
    (dbl 3)
    )
  6)

(assert-eq
  (progn
    (defmacro dbl  (x) (list '+ x x))
    (defmacro incf (y) (list 'setq y (list '+ y 1)))
    (let ((a1 2))
      (dbl (incf a1)))
    )
  7)

(assert-eq (list ''foo) (list (list 'quote 'foo)))

(assert-eq
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

(assert-eq (progn
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
(assert-eq
  (cond (nil 1) (t 2))
  2)

(assert-eq
  (cond ((equal 1 1) "a") ((equal 1 2) "b"))
  "a")

;;; test for progn
(assert-eq (progn 1) 1)
(assert-eq (progn 1 2) 2)

;;; test for quote
(assert-eq (quote (1 . 2))
      (cons 1 2))
(assert-eq (equal (list 1 (+ 1 1) (+ 1 2)) '(1 2 3))
      t)

;;; test for eq/eql/equal/equalp
(assert-eq (eq 1 1)
      t)
(assert-eq (eq 1 2)
      nil)
(assert-eq (eq (cons 1 2) (cons 1 2))
      nil)
(assert-eq (let ((a (cons 1 2))) (eq a a))
      t)
(assert-eq (eql 1 1)
      t)
(assert-eq (eql 1 2)
      nil)
(assert-eq (eql (cons 1 2) (cons 1 2))
      nil)
(assert-eq (equal (cons 1 2) (cons 1 2))
      t)
(assert-eq (let ((a (cons 1 2))) (eql a a))
      t)
(assert-eq (eql 1 1)
      t)
(assert-eq (eql 1 1.0)
      nil)
(assert-eq (equal 1 1.0)
      nil)
(assert-eq (equal "A" "A")
      t)
(assert-eq (equal "a" "A")
      nil)
(assert-eq (equalp "a" "A")
      t)
(assert-eq (equalp 1 1.0)
      t)

;;; test for (flet)
(assert-eq (flet ((f (x) (+ x 3)))
        (flet ((f (x) (+ x (f x))))
          (f 7)
          )
        )
      17)

;;; test for (labels)

(assert-eq (labels
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
(assert-eq
  (progn
    (defun cat (left &rest args)
      (apply #'string-append  left args)
      )
    (cat "1" "2" "3" "4" "5"))
  "12345")

;;; test for (apply)
(assert-eq (apply #'+ '(1 2 3))
      6)
(assert-eq (apply #'+ 4 5 6 '(1 2 3))
      21)

;;; test for (funcall)
(assert-eq (let ((f (lambda (a b) (+ a b))))
        (funcall f 1 2))
      3)

;;; test for lambda
(assert-eq (progn
        (defun f (a b)
          (+ a b))
        (f 1 2))
      3)

(assert-eq (progn
        (defun f1 (a b)
          (+ a b))
        (f1 1.0 2.0))
      3.0)

(assert-eq (let ((f2 (lambda (a b) (+ a b))))
        (funcall f2 4 5))
      9)

(assert-eq (let (a)
        (setq a 0)
        (defun dummy (a b) (+ a b))
        (dummy 7 8)
        a)
      0)

(assert-eq (progn (let ((x 1))
               (defun f ()
                 (list x)))
             (let ((x 2))
               (f)))
      (list 1))

(assert-eq (let (c)(setq c "a") c)
      "a")

(assert-eq (progn
        (defglobal c "a")
        (defun f (a)
          (let ((c "b"))
            (+ a 1)
            )
          )
        (list (f 4) c))
      (list 5 "a"))

(assert-eq (progn
        (defglobal c "a")
        (defun f (a / c)
          (setq c "b")
          (+ a 1)
          )
        (list (f 4) c)
        )
      (list 5 "a")
      )

(assert-eq (let ((a 0)) (if t (setq a 1) (setq a 2)) a)
      1)
(assert-eq (let ((x "1")) (if nil (setq x "2") (setq x "3")) x)
      "3")

(dolist (testcode (wildcard "test/*.lsp"))
  (format t "test ~s~%" testcode)
  (load testcode))
