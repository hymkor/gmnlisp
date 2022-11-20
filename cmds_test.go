package gmnlisp

import (
	"testing"
)

func TestCmdGo(t *testing.T) {
	assertEqual(t, "(quote (1 . 2))", &Cons{Car: Integer(1), Cdr: Integer(2)})
	assertEqual(t, `(equal (list 1 (+ 1 1) (+ 1 2)) '(1 2 3))`, True)
}

func TestEq(t *testing.T) {
	assertEqual(t, `(eq 1 1)`, True)
	assertEqual(t, `(eq 1 2)`, Null)
	assertEqual(t, `(eq (cons 1 2) (cons 1 2))`, Null)
	assertEqual(t, `(let ((a (cons 1 2))) (eq a a))`, True)

	assertEqual(t, `(eql 1 1)`, True)
	assertEqual(t, `(eql 1 2)`, Null)
	assertEqual(t, `(eql (cons 1 2) (cons 1 2))`, Null)
	assertEqual(t, `(equal (cons 1 2) (cons 1 2))`, True)
	assertEqual(t, `(let ((a (cons 1 2))) (eql a a))`, True)

	assertEqual(t, `(eql 1 1)`, True)
	assertEqual(t, `(eql 1 1.0)`, Null)
	assertEqual(t, `(equal 1 1.0)`, Null)
	assertEqual(t, `(equal "A" "A")`, True)
	assertEqual(t, `(equal "a" "A")`, Null)
	assertEqual(t, `(equalp "a" "A")`, True)
	assertEqual(t, `(equalp 1 1.0)`, True)
}
