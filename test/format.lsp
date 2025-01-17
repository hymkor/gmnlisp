;; test for (format)
(assert-eq (format nil "~d" 123) "123")
(assert-eq (format nil "~x" 123) "7B")
(assert-eq (format nil "~o" 123) "173")
(assert-eq (format nil "~b" 123) "1111011")
(assert-eq (format nil "~f" 12.3) "12.3")
(assert-eq (format nil "~e" 12.3) "1.23e+01")
(assert-eq (format nil "~g" 12.3) "12.3")
(assert-eq (format nil "~a" "ABC") "ABC")
(assert-eq (format nil "~s" "ABC") "\"ABC\"")
(assert-eq (format nil "[~5d]" 123) "[  123]")
(assert-eq (format nil "[~5a]" "ABC") "[ABC  ]")
(assert-eq (format nil "[~5f]" 1.3) "[  1.3]")
(assert-eq (format nil "[~5,2f]" 1.3) "[ 1.30]")

;;; test for (format-integer)
(assert-eq
  (let ((s (create-string-output-stream)))
    (format-integer s 123 10)
    (get-output-stream-string s)
    ) "123")

;;; test for (format-char)
(assert-eq
  (let ((s (create-string-output-stream)))
    (format-char s #\A)
    (format-char s #\B)
    (get-output-stream-string s)
    ) "AB")

;;; test for (format-object ... t)
(assert-eq
  (let ((s (create-string-output-stream)))
    (format-object s "ahaha" t)
    (get-output-stream-string s)
    ) "\"ahaha\"")

;;; test for (format-object ... nil)
(assert-eq
  (let ((s (create-string-output-stream)))
    (format-object s "ahaha" nil)
    (get-output-stream-string s)
    ) "ahaha")

;;; test for (format-float)
(assert-eq
  (let ((s (create-string-output-stream)))
    (format-float s 0.3)
    (get-output-stream-string s)
    ) "0.3")

;;; test for (format-float) ;;;
(assert-eq (format-float nil 3.2) "3.2")

;;; test for (format-integer) ;;;
(assert-eq (format-integer nil 100 10) "100")

;;; test for (format-object) ;;;
(assert-eq (format-object nil "ahaha" t) "\"ahaha\"")

;;; test for (format-char) ;;;
(assert-eq (format-char nil #\a) "a")

(assert-eq (format nil "~&ahaha") "ahaha")
(assert-eq (format nil "ahaha~&ahaha") (format nil "ahaha~%ahaha"))
(assert-eq (format nil "ahaha~&~&ahaha") (format nil "ahaha~%ahaha"))

;;; test for (format-tab)
(assert-eq (let ((s (create-string-output-stream)))
        (format s "A")
        (format-tab s 4)
        (format s "B")
        (get-output-stream-string s))
      "A   B")

(assert-eq (format nil "[~3T]")   "[  ]")
(assert-eq (format nil "[x~3T]")  "[x ]")
(assert-eq (format nil "[xy~3T]") "[xy]")
