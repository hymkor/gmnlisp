package gmnlisp

import (
	"testing"
)

func TestString(t *testing.T) {
	assertEqual(t, `(string-append "12345" "67890")`, UTF32String("1234567890"))
	assertEqual(t, `(strcase "abcdef")`, UTF32String("ABCDEF"))
	assertEqual(t, `(substr "1234" 2 2)`, UTF32String("23"))
	assertEqual(t, `(split-string "12345" "")`,
		List(UTF32String("1"), UTF32String("2"), UTF32String("3"), UTF32String("4"), UTF32String("5")))
}
