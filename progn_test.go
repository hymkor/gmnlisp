package gmnlisp

import (
	"context"
	"errors"
	"testing"
)

func TestIf(t *testing.T) {
	assertEqual(t, "(if t 1 2)", Integer(1))
	assertEqual(t, "(if nil 1 2)", Integer(2))
	assertEqual(t, "(if t 3)", Integer(3))
	assertEqual(t, "(if nil 3)", Null)

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

	assertEqual(t, `
	(let (aa)
		(cond
			((= (setq aa (string-append "a" "a")) "ab") "A")
			((= aa "aa") "B")
			(T "fail")
		)
	)`, String("B"))
}

func TestCase(t *testing.T) {
	assertEqual(t, `
		(case 2
			((1) "a")
			((2) "b")
			((3) "c")
		)`, String("b"))

	assertEqual(t, `
		(case 4
			((1 2) "A")
			((3 4) "B")
			((5 6) "C")
		)`, String("B"))

	assertEqual(t, `
		(case 7
			((1 2) "A")
			((3 4) "B")
			(t "C")
		)`, String("C"))
}

func TestWithHandler(t *testing.T) {
	assertEqual(t, `
		(catch 'hoge
			(with-handler
				(lambda (c)
				  (if (eql c *err-variable-unbound*)
					(throw 'hoge "OK")))
				(not-exist-func)
				"NG"
			)
		)`, String("OK"))

	assertEqual(t, `
		(catch 'hoge
			(with-handler
				(lambda (c)
				  (if (eql c *err-variable-unbound*)
					(throw 'hoge "OK")))
				;(not-exist-func)
				"NG"
			)
		)`, String("NG"))
}

func TestCatch(t *testing.T) {
	assertEqual(t, `
		(defun foo (x)
			(catch 'block-sum (bar x))
		)
		(defun bar (x)
			(let (sum L)
				(for ((L x (cdr L))
					(sum 0 (+ sum (car L))))
				((null L) sum)
					(cond
						((not (numberp (car L))) (throw 'block-sum 0))
					)
				)
			)
		)
		(and
			(equal 10 (foo '(1 2 3 4)))
			(equal  0 (foo '(1 2 nil 4)))
		)`, True)
}

func TestUnwindProtect(t *testing.T) {
	assertEqual(t, `
		(let ((x 0))
			(and
				(equalp
					(catch 'hogehoge
						(defun foo ()
							(throw 'hogehoge 10)
						)
						(unwind-protect
							(foo)
							(setq x 1)
						)
					)
					10
				)
				(equalp x 1)
			)
		)
	`, True)
}
