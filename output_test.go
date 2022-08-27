package gmnlisp

import (
	"testing"
)

func TestFormat(t *testing.T) {
	assertEqual(t, `
		(let ((s (create-string-output-stream)))
			(format-integer s 123 10)
			(get-output-stream-string s)
		)`, UTF32String("123"))
	assertEqual(t, `(format nil "~d" 123)`, UTF32String("123"))
	assertEqual(t, `(format nil "~x" 123)`, UTF32String("7B"))
	assertEqual(t, `(format nil "~o" 123)`, UTF32String("173"))
	assertEqual(t, `(format nil "~b" 123)`, UTF32String("1111011"))
	assertEqual(t, `(format nil "~f" 12.3)`, UTF32String("12.3"))
	assertEqual(t, `(format nil "~e" 12.3)`, UTF32String("1.23e+01"))
	assertEqual(t, `(format nil "~g" 12.3)`, UTF32String("12.3"))
	assertEqual(t, `(format nil "~a" "ABC")`, UTF32String("ABC"))
	assertEqual(t, `(format nil "~s" "ABC")`, UTF32String(`"ABC"`))
	assertEqual(t, `(format nil "[~5d]" 123)`, UTF32String(`[  123]`))
	assertEqual(t, `(format nil "[~5a]" "ABC")`, UTF32String(`[ABC  ]`))
}
