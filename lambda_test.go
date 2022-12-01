package gmnlisp

import (
	"context"
	"errors"
	"testing"
)

func TestLambdaGo(t *testing.T) {
	w := New()
	ctx := context.TODO()
	_, err := w.Interpret(ctx, `(defun f (x y) (+ x y))`)
	if err != nil {
		t.Fatal(err.Error())
	}
	_, err = w.Interpret(ctx, `(f 1)`)
	if !errors.Is(err, ErrTooFewArguments) {
		t.Fatal("Few argumenets error did not occur")
	}

	_, err = w.Interpret(ctx, `(f 1 2)`)
	if err != nil {
		t.Fatal(err.Error())
	}

	_, err = w.Interpret(ctx, `(f 1 2 3)`)
	if !errors.Is(err, ErrTooManyArguments) {
		t.Fatal("Too Many argumenets error did not occur")
	}
}
