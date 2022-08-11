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

func TestDoList(t *testing.T) {
	assertEqual(t, `(let (n (sum 0))
		(dolist (n '(1 3 5 7))
			(setq sum (+ sum n)))
		sum)`, Integer(1+3+5+7))
}

func TestWhen(t *testing.T) {
	assertEqual(t, `(let ((a 0)) (when (> 5 3) (setq a 1)) a)`, Integer(1))
	assertEqual(t, `(let ((a 0)) (when (< 5 3) (setq a 1)) a)`, Integer(0))
}

func TestUnless(t *testing.T) {
	assertEqual(t, `(let ((a 0)) (unless (> 5 3) (setq a 1)) a)`, Integer(0))
	assertEqual(t, `(let ((a 0)) (unless (< 5 3) (setq a 1)) a)`, Integer(1))
}

func TestCase(t *testing.T) {
	assertEqual(t, `
		(case 2
			(1 "a")
			(2 "b")
			(3 "c")
		)`, String("b"))

	assertEqual(t, `
		(case 4
			((1 2) "A")
			((3 4) "B")
			((5 6) "C")
		)`, String("B"))
}

func TestLoop(t *testing.T) {
	assertEqual(t, `
		(let ((i 0))
			(loop
				(incf i)
				(if (> i 5)
					(return (* i 10)))))`, Integer(60))
}

func TestHandlerCase(t *testing.T) {
	assertEqual(t, `
		(handler-case (with-open-file "not-exists")
			(error (c)
				c)
			(:no-error
				"SUCCESS")
			)`, &ErrorNode{Value: ErrTooFewArguments})

	assertEqual(t, `
		(handler-case (+ 1 2)
			(error ()
				"ERROR FOUND")
			(:no-error (c)
				c)
		)`, Integer(3))
}
