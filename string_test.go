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

func TestConcatenate(t *testing.T) {
	assertEqual(t, `(concatenate 'string "123" "456")`, String("123456"))
	assertEqual(t, `(concatenate 'list '(1 2 3) '(4 5 6))`,
		List(Integer(1), Integer(2), Integer(3), Integer(4), Integer(5), Integer(6)))
}
