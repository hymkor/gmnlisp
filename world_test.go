package gmnlisp

import (
	"errors"
	"testing"
)

func evalTest(t *testing.T, equation string, expect Node) {
	w := New()
	result, err := w.Interpret(equation)
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
	evalTest(t, `(> "a" "b")`, Null)
	evalTest(t, `(< "a" "b" "c")`, True)
	evalTest(t, `(< 1 2 3)`, True)
	evalTest(t, `(< 1 2 1)`, Null)
	evalTest(t, `(>= 3 2 2)`, True)
	evalTest(t, `(>= 2 2 2)`, True)
	evalTest(t, `(> 3 2 1)`, True)
	evalTest(t, `(= 1 1)`, True)
	evalTest(t, `(= 1.0 1)`, True)
	evalTest(t, `(= 1 1.0)`, True)
	evalTest(t, `(= 1.0 1.0)`, True)
	evalTest(t, `(= 1.0 1.0 1.0)`, True)
	evalTest(t, `(= 1 2)`, Null)
	evalTest(t, `(= 1 2.0)`, Null)
	evalTest(t, `(= 1.0 2)`, Null)
	evalTest(t, `(= 1 2.0)`, Null)
	evalTest(t, `(= "ABC" "abc")`, True)
	evalTest(t, `(= "ABC" "abcd")`, Null)
	evalTest(t, `(equalp "DEF" "defg")`, Null)
	evalTest(t, `(equalp "GHQ" "ghq")`, True)
	evalTest(t, `(equalp (cons 1 (cons 2 nil)) '(1 2))`, True)
	evalTest(t, `(equalp (cons 1 2) '(1))`, Null)
}

func TestCmdCond(t *testing.T) {
	evalTest(t, `(cond (nil 1) (T 2))`, Integer(2))
	evalTest(t, `(cond ((equal 1 1) "a") ((equal 1 2) "b"))`, String("a"))
}

func TestProgn(t *testing.T) {
	evalTest(t, `(progn 1)`, Integer(1))
	evalTest(t, `(progn 1 2)`, Integer(2))
}

func TestWorld(t *testing.T) {
	w1 := New()
	if _, err := w1.Interpret(`(setq a "A")`); err != nil {
		t.Fatal(err.Error())
		return
	}
	value, err := w1.Interpret("a")
	if err != nil {
		t.Fatal(err.Error())
		return
	}
	s, ok := value.(String)
	if !ok {
		t.Fatal("type mismatch")
		return
	}
	if string(s) != "A" {
		t.Fatalf("`%s` != `A`", string(s))
		return
	}

	w2 := New()
	value, err = w2.Interpret(`a`)

	if !errors.Is(err, ErrVariableUnbound) {
		if err == nil {
			t.Fatal("error had to occur")
		} else {
			t.Fatal(err.Error())
		}
		return
	}
}

func TestTokenizer(t *testing.T) {
	evalTest(t, `
		(list 1 2 ;
		  3;
		  4)`,
		List(Integer(1),
			Integer(2),
			Integer(3),
			Integer(4)))
}
