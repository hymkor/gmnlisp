package gmnlisp

import (
	"testing"
)

func evalTest(t *testing.T, equation string, expect Node) {
	result, err := Interpret(equation)
	if err != nil {
		t.Fatal(err.Error())
		return
	}
	if !result.Equals(expect) {
		t.Fatalf("%s != %s (was %s)", equation, Node2String(expect), Node2String(result))
		return
	}
}

func TestOperators(t *testing.T) {
	evalTest(t, "(+ 1 2)", NodeInteger(3))
	evalTest(t, "(+ 1 2 3)", NodeInteger(6))
	evalTest(t, "(- 10 9)", NodeInteger(1))
	evalTest(t, "(- 10 1 2)", NodeInteger(7))
	evalTest(t, "(* 1 2)", NodeInteger(2))
	evalTest(t, "(* 1 2 3)", NodeInteger(6))
	evalTest(t, "(/ 6 2)", NodeInteger(3))
	evalTest(t, `(+ "1" "2")`, NodeString("12"))
}

func TestCmdCond(t *testing.T) {
	evalTest(t, `(cond (nil 1) (T 2))`, NodeInteger(2))
	evalTest(t, `(cond ((equal 1 1) "a") ((equal 1 2) "b"))`, NodeString("a"))
}

func TestCmdCons(t *testing.T) {
	evalTest(t, "(cons 1 2)", &Cons{Car: NodeInteger(1), Cdr: NodeInteger(2)})
	evalTest(t, "(quote (1 . 2))", &Cons{Car: NodeInteger(1), Cdr: NodeInteger(2)})
}

func TestProgn(t *testing.T) {
	evalTest(t, `(progn 1)`, NodeInteger(1))
	evalTest(t, `(progn 1 2)`, NodeInteger(2))
}
