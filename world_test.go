package gmnlisp

import (
	"context"
	"errors"
	"math"
	"testing"
	"time"
)

func assertEqual(t *testing.T, equation string, expect Node) {
	w := New()
	if e := w.Assert(equation, expect); e != "" {
		t.Helper()
		t.Fatal(e)
	}
}

func TestWorld(t *testing.T) {
	w1 := New()
	if _, err := w1.Interpret(context.TODO(), `(defglobal a "A")`); err != nil {
		t.Fatal(err.Error())
		return
	}
	value, err := w1.Interpret(context.TODO(), "a")
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
	_, err = w2.Interpret(context.TODO(), `a`)

	if !errors.Is(err, ErrVariableUnbound) {
		if err == nil {
			t.Fatal("error had to occur")
		} else {
			t.Fatal(err.Error())
		}
		return
	}

	w3 := New()
	_, err = w3.Interpret(context.TODO(), "( 1")
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

func TestContextWhile(t *testing.T) {
	ctx, cancel := context.WithTimeout(
		context.Background(),
		100*time.Millisecond)
	w := New()
	_, err := w.Interpret(ctx, `
		(let* ((bs (create-string 1 (convert 8 <character>)))
		       (s (string-append " " bs)))
			(while t
				(format (standard-output) s)
			)
		)`)
	cancel()
	if !errors.Is(err, context.DeadlineExceeded) {
		if err == nil {
			t.Fatal("time out did not work(err=nil)")
		} else {
			t.Fatalf("time out did not work(err=%s)", err.Error())
		}
	}
}

func TestContextLambda(t *testing.T) {
	ctx, cancel := context.WithTimeout(
		context.Background(),
		10*time.Millisecond)
	w := New()
	_, err := w.Interpret(ctx, `
		(defun foo ()
			(let* ((bs (create-string 1 (convert 8 <character>)))
				   (s (string-append " " bs)))
				(format (standard-output) s)
				(foo)
			)
		)
		(foo)`)
	cancel()

	if !errors.Is(err, context.DeadlineExceeded) {
		if err == nil {
			t.Fatal("time out did not work(err=nil)")
		} else {
			t.Fatalf("time out did not work(err=%s)", err.Error())
		}
	}
}

func TestIf(t *testing.T) {
	w := New()
	ctx := context.TODO()
	_, err := w.Interpret(ctx, "(if t 1 2 3)")
	if !errors.Is(err, ErrTooManyArguments) {
		t.Fatal("ErrTooManyArguments have to be occured")
	}
	_, err = w.Interpret(ctx, "(if)")
	if !errors.Is(err, ErrTooFewArguments) {
		t.Fatal("ErrTooFewArguments have to be occured")
	}
}

var _ CanKnowLastOutput = &_WriterNode{}

func testFloat(t *testing.T, text string, expected float64) {
	t.Helper()

	w := New()
	ctx := context.TODO()
	val, err := w.Interpret(ctx, text)
	if err != nil {
		t.Fatal(err.Error())
	}
	value, ok := val.(Float)
	if !ok {
		t.Fatalf("Not a float number: '%v'", val)
	}
	value_ := float64(value)
	epsilon := math.Abs(expected) * 1e-6
	if value_ < expected-epsilon || expected+epsilon < value_ {
		t.Fatalf("expected '%v' but '%v'", expected, value_)
	}
}

func TestFloat(t *testing.T) {
	testFloat(t, "1.0", 1.0)
	testFloat(t, "10.", 10.0)
	testFloat(t, "1.0E1", 10.0)
	testFloat(t, "-1.0e-1", -0.1)
	testFloat(t, "7e3", 7000)
	testFloat(t, "-7e-3", -0.007)
}

func testInt(t *testing.T, text string, expected int) {
	t.Helper()

	w := New()
	ctx := context.TODO()
	val, err := w.Interpret(ctx, text)
	if err != nil {
		t.Fatal(err.Error())
	}
	value, ok := val.(Integer)
	if !ok {
		t.Fatalf("Not a integer: '%v'", val)
	}
	value_ := int(value)
	if value_ != expected {
		t.Fatalf("expected '%v' but '%v'", expected, value_)
	}
}

func TestInt(t *testing.T) {
	testInt(t, "1", 1)
	testInt(t, "-10", -10)
	testInt(t, "#xB", 11)
	testInt(t, "#X1c", 16+12)
}
