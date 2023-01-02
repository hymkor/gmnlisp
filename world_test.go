package gmnlisp

import (
	"context"
	"errors"
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
	value, err = w2.Interpret(context.TODO(), `a`)

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
