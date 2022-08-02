package gmnlisp

import (
	"testing"
)

func TestString(t *testing.T) {
	assertEqual(t, `(strcat "12345" "67890")`, String("1234567890"))
	assertEqual(t, `(strlen "12345")`, Integer(5))
	assertEqual(t, `(strcase "abcdef")`, String("ABCDEF"))
	assertEqual(t, `(substr "1234" 2 2)`, String("23"))
	assertEqual(t, `(split-string "12345" "")`,
		List(String("1"), String("2"), String("3"), String("4"), String("5")))
}
