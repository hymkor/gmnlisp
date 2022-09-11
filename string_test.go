package gmnlisp

import (
	"testing"
)

func TestStringOperators(t *testing.T) {
	assertEqual(t, `(string< "a" "b")`, True)
	assertEqual(t, `(string> "a" "b")`, Null)
	assertEqual(t, `(string<= "a" "b")`, True)
	assertEqual(t, `(string>= "a" "b")`, Null)
	assertEqual(t, `(string= "a" "b")`, Null)
	assertEqual(t, `(string/= "a" "b")`, True)
}
