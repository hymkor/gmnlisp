package gmnlisp

import (
	"testing"
)

func TestFormat(t *testing.T) {
	assertEqual(t, `(format nil "~d" 123)`, String("123"))
	assertEqual(t, `(format nil "~x" 123)`, String("7B"))
	assertEqual(t, `(format nil "~o" 123)`, String("173"))
	// assertEqual(t, `(format nil "~b" 123)`, String("1111011"))
	// assertEqual(t, `(format nil "~f" 12.3)`, String("12.3"))
	// (format nil "~e" 12.3)	;=> 1.23e+1 ... 指数形式浮動小数指示
	// (format nil "~g" 12.3)	;=> 12.3 ... ~f または ~e
	assertEqual(t, `(format nil "~a" "ABC")`, String("ABC"))
	assertEqual(t, `(format nil "~s" "ABC")`, String(`"ABC"`))
}
