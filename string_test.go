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

func TestStringIndex(t *testing.T) {
	assertEqual(t, `(string-index "foo" "foobar")`, Integer(0))
	assertEqual(t, `(string-index "bar" "foobar")`, Integer(3))
	assertEqual(t, `(string-index "FOO" "foobar")`, Null)
	assertEqual(t, `(string-index "foo" "foobar" 1)`, Null)
	assertEqual(t, `(string-index "bar" "foobar" 1)`, Integer(3))
	assertEqual(t, `(string-index "foo" "")`, Null)
	assertEqual(t, `(string-index "" "foo")`, Integer(0))
}

func TestCreateString(t *testing.T) {
	assertEqual(t, `(create-string 1 #\A)`, String("A"))
	assertEqual(t, `(create-string 2 #\B)`, String("BB"))
}
