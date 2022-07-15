package gmnlisp

import (
	"testing"
)

func TestCmdGo(t *testing.T) {
	assertEqual(t, "(cons 1 2)", &Cons{Car: Integer(1), Cdr: Integer(2)})
	assertEqual(t, "(car '(1 2))", Integer(1))
	assertEqual(t, "(car '(1 . 2))", Integer(1))
	assertEqual(t, "(cdr '(1 . 2))", Integer(2))
	assertEqual(t, "(cdr '(1 2))", List(Integer(2)))
	assertEqual(t, "(quote (1 . 2))", &Cons{Car: Integer(1), Cdr: Integer(2)})
	assertEqual(t, "(atom 1)", True)
	assertEqual(t, "(atom '(1 2))", Null)

	assertEqual(t, `(equal (list 1 (+ 1 1) (+ 1 2)) '(1 2 3))`, True)
	assertEqual(t, `(list 1 2 3 4)`,
		List(Integer(1), Integer(2), Integer(3), Integer(4)))
	assertEqual(t, `(append '(1 2) '(3 4))`, List(Integer(1), Integer(2), Integer(3), Integer(4)))
	assertEqual(t, `(append '(1 2) '(3 4) '(5 6))`,
		List(Integer(1), Integer(2), Integer(3), Integer(4), Integer(5), Integer(6)))
}
