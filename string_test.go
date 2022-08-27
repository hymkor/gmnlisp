package gmnlisp

import (
	"testing"
)

func TestString(t *testing.T) {
	assertEqual(t, `(string-append "12345" "67890")`, String("1234567890"))
	assertEqual(t, `(strcase "abcdef")`, String("ABCDEF"))
	assertEqual(t, `(split-string "12345" "")`,
		List(String("1"), String("2"), String("3"), String("4"), String("5")))
}
