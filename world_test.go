package gmnlisp

import (
	"errors"
	"testing"
)

func assertEqual(t *testing.T, equation string, expect Node) {
	w := New()
	result, err := w.Interpret(equation)
	if err != nil {
		t.Fatalf("%s: %s", equation, err.Error())
		return
	}
	if !result.Equals(expect, EQUAL) {
		t.Fatalf("%s != %s (was %s)", equation, toString(expect), toString(result))
		return
	}
}

func TestCmdCond(t *testing.T) {
	assertEqual(t, `(cond (nil 1) (T 2))`, Integer(2))
	assertEqual(t, `(cond ((equal 1 1) "a") ((equal 1 2) "b"))`, String("a"))
}

func TestProgn(t *testing.T) {
	assertEqual(t, `(progn 1)`, Integer(1))
	assertEqual(t, `(progn 1 2)`, Integer(2))
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

	w3 := New()
	_, err = w3.Interpret("( 1")
	if !errors.Is(err, ErrTooShortTokens) {
		if err == nil {
			t.Fatal("error had to occur when equation is not closed")
		} else {
			t.Fatal(err.Error())
		}
	}
}

func TestTokenizer(t *testing.T) {
	assertEqual(t, `
		(list 1 2 ;
		  3;
		  4)`,
		List(Integer(1),
			Integer(2),
			Integer(3),
			Integer(4)))
}
