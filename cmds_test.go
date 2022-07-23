package gmnlisp

import (
	"testing"
)

func TestCmdGo(t *testing.T) {
	assertEqual(t, "(quote (1 . 2))", &Cons{Car: Integer(1), Cdr: Integer(2)})
	assertEqual(t, "(atom 1)", True)
	assertEqual(t, "(atom '(1 2))", Null)

	assertEqual(t, `(equal (list 1 (+ 1 1) (+ 1 2)) '(1 2 3))`, True)
	assertEqual(t, `(listp ())`, True)
	assertEqual(t, `(listp 1)`, Null)
	assertEqual(t, `(listp '(1 2 3))`, True)
	assertEqual(t, `(not (/= 1 1))`, True)
	assertEqual(t, `(not (= 1 1))`, Null)
	assertEqual(t, `(/= 1 2)`, True)
	assertEqual(t, `(/= 1 1)`, Null)
}
