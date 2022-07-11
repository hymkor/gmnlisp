package gmnlisp

import (
	"testing"
)

func evalTest(t *testing.T, equation string, expect Node) {
	ins := New()
	result, err := ins.Interpret(equation)
	if err != nil {
		t.Fatalf("%s: %s", equation, err.Error())
		return
	}
	if !result.Equals(expect) {
		t.Fatalf("%s != %s (was %s)", equation, toString(expect), toString(result))
		return
	}
}

func TestOperators(t *testing.T) {
	evalTest(t, "(+ 1 2)", Integer(3))
	evalTest(t, "(+ 1 2 3)", Integer(6))
	evalTest(t, "(- 10 9)", Integer(1))
	evalTest(t, "(- 10 1 2)", Integer(7))
	evalTest(t, "(* 1 2)", Integer(2))
	evalTest(t, "(* 1 2 3)", Integer(6))
	evalTest(t, "(/ 6 2)", Integer(3))
	evalTest(t, `(+ "1" "2")`, String("12"))
	evalTest(t, `(> 2 1.0)`, True)
	evalTest(t, `(> 2.0 3)`, Null)
	evalTest(t, `(< 2.0 3)`, True)
	evalTest(t, `(< 2 1.0)`, Null)
	evalTest(t, `(<= 2.0 3)`, True)
	evalTest(t, `(<= 3 3)`, True)
	evalTest(t, `(<= 4 3)`, Null)
	evalTest(t, `(>= 2.0 3)`, Null)
	evalTest(t, `(>= 3 3)`, True)
	evalTest(t, `(>= 4 3)`, True)
	evalTest(t, `(< "a" "b")`, True)
	evalTest(t, `(> "a" "b")`, Null)
}

func TestCmdCond(t *testing.T) {
	evalTest(t, `(cond (nil 1) (T 2))`, Integer(2))
	evalTest(t, `(cond ((equal 1 1) "a") ((equal 1 2) "b"))`, String("a"))
}

func TestCmdCons(t *testing.T) {
	evalTest(t, "(cons 1 2)", &Cons{Car: Integer(1), Cdr: Integer(2)})
	evalTest(t, "(quote (1 . 2))", &Cons{Car: Integer(1), Cdr: Integer(2)})
}

func TestProgn(t *testing.T) {
	evalTest(t, `(progn 1)`, Integer(1))
	evalTest(t, `(progn 1 2)`, Integer(2))
}
