package gmnlisp

import (
	"errors"
	"testing"
)

func TestIf(t *testing.T) {
	assertEqual(t, "(if T 1 2)", Integer(1))
	assertEqual(t, "(if nil 1 2)", Integer(2))
	assertEqual(t, "(if T 3)", Integer(3))
	assertEqual(t, "(if nil 3)", Null)

	w := New()
	_, err := w.Interpret("(if T 1 2 3)")
	if !errors.Is(err, ErrTooManyArguments) {
		t.Fatal("ErrTooManyArguments have to be occured")
	}
	_, err = w.Interpret("(if)")
	if !errors.Is(err, ErrTooFewArguments) {
		t.Fatal("ErrTooFewArguments have to be occured")
	}

	assertEqual(t, `
	(cond
		((= (setq aa (strcat "a" "a")) "ab") "A")
		((= aa "aa") "B")
		(T "fail"))`, String("B"))
}
