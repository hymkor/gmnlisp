package gmnlisp

import (
	"testing"
)

func TestVector(t *testing.T) {
	assertEqual(t, `(elt (vector "a" "b" "c") 1)`, String("b"))
	assertEqual(t, `(subseq (vector "a" "b" "c" "d" "e" "f") 1 4)`,
		NewVector(String("b"), String("c"), String("d")))
	assertEqual(t, `#(1 2 3)`, NewVector(Integer(1), Integer(2), Integer(3)))
}
