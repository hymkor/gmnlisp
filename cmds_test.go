package gmnlisp

import (
	"testing"
)

func TestCmdGo(t *testing.T) {
	evalTest(t, "(cons 1 2)", &Cons{Car: Integer(1), Cdr: Integer(2)})
	evalTest(t, "(car '(1 2))", Integer(1))
	evalTest(t, "(car '(1 . 2))", Integer(1))
	evalTest(t, "(cdr '(1 . 2))", Integer(2))
	evalTest(t, "(cdr '(1 2))", List(Integer(2)))
	evalTest(t, "(quote (1 . 2))", &Cons{Car: Integer(1), Cdr: Integer(2)})
	evalTest(t, "(atom 1)", True)
	evalTest(t, "(atom '(1 2))", Null)

	evalTest(t, `(equal (list 1 (+ 1 1) (+ 1 2)) '(1 2 3))`, True)
	evalTest(t, `(list 1 2 3 4)`,
		List(Integer(1), Integer(2), Integer(3), Integer(4)))
	evalTest(t, `(append '(1 2) '(3 4))`, List(Integer(1), Integer(2), Integer(3), Integer(4)))
	evalTest(t, `(append '(1 2) '(3 4) '(5 6))`,
		List(Integer(1), Integer(2), Integer(3), Integer(4), Integer(5), Integer(6)))
}
