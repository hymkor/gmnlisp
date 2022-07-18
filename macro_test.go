package gmnlisp

import (
	"testing"
)

func TestMacro(t *testing.T) {
	assertEqual(t,`
		(defmacro dbl (x) (list '* x 2))
		(dbl 3)
	`, Integer(6))

	assertEqual(t,`
		(defmacro dbl  (x) (list '+ x x))
		(defmacro incf (x) (setq x (+ x 1)))
		(let ((a1 2))
			(dbl (incf a1)))
	`, Integer(7))
}
