;; test for (format)
(test (format nil "~d" 123) "123")
(test (format nil "~x" 123) "7B")
(test (format nil "~o" 123) "173")
(test (format nil "~b" 123) "1111011")
(test (format nil "~f" 12.3) "12.3")
(test (format nil "~e" 12.3) "1.23e+01")
(test (format nil "~g" 12.3) "12.3")
(test (format nil "~a" "ABC") "ABC")
(test (format nil "~s" "ABC") "\"ABC\"")
(test (format nil "[~5d]" 123) "[  123]")
(test (format nil "[~5a]" "ABC") "[ABC  ]")
(test (format nil "[~5f]" 1.3) "[  1.3]")
(test (format nil "[~5,2f]" 1.3) "[ 1.30]")
(test (format nil "~5%") (create-string 5 #\newline))

;;; test for (format-integer)
(test
  (let ((s (create-string-output-stream)))
    (format-integer s 123 10)
    (get-output-stream-string s)
    ) "123")

;;; test for (format-char)
(test
  (let ((s (create-string-output-stream)))
    (format-char s #\A)
    (format-char s #\B)
    (get-output-stream-string s)
    ) "AB")

;;; test for (format-object ... t)
(test
  (let ((s (create-string-output-stream)))
    (format-object s "ahaha" t)
    (get-output-stream-string s)
    ) "\"ahaha\"")

;;; test for (format-object ... nil)
(test
  (let ((s (create-string-output-stream)))
    (format-object s "ahaha" nil)
    (get-output-stream-string s)
    ) "ahaha")

;;; test for (format-float)
(test
  (let ((s (create-string-output-stream)))
    (format-float s 0.3)
    (get-output-stream-string s)
    ) "0.3")

;;; test for (format-float) ;;;
(test (format-float nil 3.2) "3.2")

;;; test for (format-integer) ;;;
(test (format-integer nil 100 10) "100")

;;; test for (format-object) ;;;
(test (format-object nil "ahaha" t) "\"ahaha\"")

;;; test for (format-char) ;;;
(test (format-char nil #\a) "a")

(test (format nil "~&ahaha") "ahaha")
(test (format nil "~2&ahaha") (format nil "~%ahaha"))
(test (format nil "ahaha~&ahaha") (format nil "ahaha~%ahaha"))

;;; test for (format-tab)
(test (let ((s (create-string-output-stream)))
        (format s "A")
        (format-tab s 4)
        (format s "B")
        (get-output-stream-string s))
      "A   B")
