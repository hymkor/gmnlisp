package gmnlisp

import (
	"testing"
)

func TestMacro(t *testing.T) {
	assertEqual(t, `
		(defmacro dbl (x) (list '* x 2))
		(dbl 3)
	`, Integer(6))

	assertEqual(t, `
		(defmacro dbl  (x) (list '+ x x))
		(defmacro incf (y) (list 'setq y (list '+ y 1)))
		(let ((a1 2))
			(dbl (incf a1)))
	`, Integer(7))

	assertEqual(t, `(list ''foo)`, List(List(NewSymbol("quote"), NewSymbol("foo"))))
}

func TestMacroExpand(t *testing.T) {
	assertEqual(t, `
		(defmacro dbl  (x) (list '+ x x))
		(defmacro incf (y) (list 'setq y (list '+ y 1)))
		(let ((a1 2))
			(macroexpand '(dbl (incf a1))))
	`, List(NewSymbol("+"), List(NewSymbol("incf"), NewSymbol("a1")),
		List(NewSymbol("incf"), NewSymbol("a1"))))
}
