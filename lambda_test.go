package gmnlisp

import (
	"errors"
	"testing"
)

func TestLambdaGo(t *testing.T) {
	assertEqual(t, `
		(progn
			(defun f (a b)
				(+ a b))
			(f 1 2))`, Integer(3))
	assertEqual(t, `
		(progn
			(defun f1 (a b)
				(+ a b))
			(f1 1.0 2.0))`, Float(3.0))
	assertEqual(t, `
		(let (
				(f2 (lambda (a b) (+ a b)))
			)
			(f2 4 5))`, Integer(9))

	assertEqual(t, `
		(progn
			(setq a 0)
			(defun dummy (a b) (+ a b))
			(dummy 7 8)
			a)`, Integer(0))

	assertEqual(t, `
		(let ((x 1))
		  (defun f ()
			(list x)))
		(let ((x 2))
			(f))`,
		&Cons{Car: Integer(1), Cdr: Null})

	assertEqual(t, `(setq c "a") c`, String("a"))

	assertEqual(t, `
		(setq c "a")
		(defun f (a)
			(let ((c "b"))
				(+ a 1)
			)
		)
		(list (f 4) c)`,
		List(Integer(5), String("a")))

	assertEqual(t, `
		(setq c "a")
		(defun f (a / c)
			(setq c "b")
			(+ a 1)
		)
		(list (f 4) c)`,
		List(Integer(5), String("a")))

	assertEqual(t, `(let ((a 0)) (if T (setq a 1) (setq a 2)) a)`, Integer(1))
	assertEqual(t, `(let ((x "1")) (if nil (setq x "2") (setq x "3")) x)`, String("3"))

	w := New()
	_, err := w.Interpret(`(defun f (x y) (+ x y))`)
	if err != nil {
		t.Fatal(err.Error())
	}
	_, err = w.Interpret(`(f 1)`)
	if !errors.Is(err, ErrTooFewArguments) {
		t.Fatal("Few argumenets error did not occur")
	}

	_, err = w.Interpret(`(f 1 2)`)
	if err != nil {
		t.Fatal(err.Error())
	}

	_, err = w.Interpret(`(f 1 2 3)`)
	if !errors.Is(err, ErrTooManyArguments) {
		t.Fatal("Few argumenets error did not occur")
	}
}
