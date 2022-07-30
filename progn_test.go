package gmnlisp

import (
	"context"
	"errors"
	"testing"
)

func TestIf(t *testing.T) {
	assertEqual(t, "(if T 1 2)", Integer(1))
	assertEqual(t, "(if nil 1 2)", Integer(2))
	assertEqual(t, "(if T 3)", Integer(3))
	assertEqual(t, "(if nil 3)", Null)

	w := New()
	ctx := context.TODO()
	_, err := w.Interpret(ctx, "(if T 1 2 3)")
	if !errors.Is(err, ErrTooManyArguments) {
		t.Fatal("ErrTooManyArguments have to be occured")
	}
	_, err = w.Interpret(ctx, "(if)")
	if !errors.Is(err, ErrTooFewArguments) {
		t.Fatal("ErrTooFewArguments have to be occured")
	}

	assertEqual(t, `
	(defvar aa)
	(cond
		((= (setq aa (strcat "a" "a")) "ab") "A")
		((= aa "aa") "B")
		(T "fail"))`, String("B"))

	assertEqual(t, `(let ((x 0)(i 0))
						(foreach i (list 1 (+ 1 1) (* 1 3) 4 5)
							(setq x (+ x i))
						)
						x
					)`, Integer(15))
}

func TestDoTimes(t *testing.T) {
	assertEqual(t, `(let (n (sum 0))
		(dotimes (n 4) (setq sum (+ sum n)))
		sum)`, Integer(0+1+2+3))
}
