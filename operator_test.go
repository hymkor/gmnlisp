package gmnlisp

import (
	"testing"
)

func TestOperators(t *testing.T) {
	assertEqual(t, "(+ 1 2)", Integer(3))
	assertEqual(t, "(+ 1 2 3)", Integer(6))
	assertEqual(t, "(- 10 9)", Integer(1))
	assertEqual(t, "(- 10 1 2)", Integer(7))
	assertEqual(t, "(* 1 2)", Integer(2))
	assertEqual(t, "(* 1 2 3)", Integer(6))
	assertEqual(t, "(/ 6 2)", Integer(3))
	assertEqual(t, `(+ "1" "2")`, String("12"))
	assertEqual(t, `(> 2 1.0)`, True)
	assertEqual(t, `(> 2.0 3)`, Null)
	assertEqual(t, `(< 2.0 3)`, True)
	assertEqual(t, `(< 2 1.0)`, Null)
	assertEqual(t, `(<= 2.0 3)`, True)
	assertEqual(t, `(<= 3 3)`, True)
	assertEqual(t, `(<= 4 3)`, Null)
	assertEqual(t, `(>= 2.0 3)`, Null)
	assertEqual(t, `(>= 3 3)`, True)
	assertEqual(t, `(>= 4 3)`, True)
	assertEqual(t, `(> "a" "b")`, Null)
	assertEqual(t, `(< "a" "b" "c")`, True)
	assertEqual(t, `(< 1 2 3)`, True)
	assertEqual(t, `(< 1 2 1)`, Null)
	assertEqual(t, `(>= 3 2 2)`, True)
	assertEqual(t, `(>= 2 2 2)`, True)
	assertEqual(t, `(> 3 2 1)`, True)
	assertEqual(t, `(= 1 1)`, True)
	assertEqual(t, `(= 1.0 1)`, True)
	assertEqual(t, `(= 1 1.0)`, True)
	assertEqual(t, `(= 1.0 1.0)`, True)
	assertEqual(t, `(= 1.0 1.0 1.0)`, True)
	assertEqual(t, `(= 1 2)`, Null)
	assertEqual(t, `(= 1 2.0)`, Null)
	assertEqual(t, `(= 1.0 2)`, Null)
	assertEqual(t, `(= 1 2.0)`, Null)
	assertEqual(t, `(= "ABC" "abc")`, True)
	assertEqual(t, `(= "ABC" "abcd")`, Null)
	assertEqual(t, `(equalp "DEF" "defg")`, Null)
	assertEqual(t, `(equalp "GHQ" "ghq")`, True)
	assertEqual(t, `(equalp (cons 1 (cons 2 nil)) '(1 2))`, True)
	assertEqual(t, `(equalp (cons 1 2) '(1))`, Null)
	assertEqual(t, `(and 1)`, Integer(1))
	assertEqual(t, `(and 1 2)`, Integer(2))
	assertEqual(t, `(and 1 2 3)`, Integer(3))
	assertEqual(t, `(and 1 nil 3)`, Null)
	assertEqual(t, `(or 1)`, Integer(1))
	assertEqual(t, `(or 1 2)`, Integer(1))
	assertEqual(t, `(or 1 2 3)`, Integer(1))
	assertEqual(t, `(or 1 nil 3)`, Integer(1))
	assertEqual(t, `(or nil 3)`, Integer(3))
	assertEqual(t, `(1+ 10)`, Integer(11))
	assertEqual(t, `(1- 10)`, Integer(9))
}

func TestIncf(t *testing.T) {
	assertEqual(t, `(let ((x 1)) (incf x) x)`, Integer(2))
	assertEqual(t, `(let ((x 1.0)) (incf x) x)`, Float(2.0))
	assertEqual(t, `(let ((x 1)) (incf x 2) x)`, Integer(3))
	assertEqual(t, `(let ((x 1.0)) (incf x 2) x)`, Float(3.0))
}

func TestDecf(t *testing.T) {
	assertEqual(t, `(let ((x 1)) (decf x) x)`, Integer(0))
	assertEqual(t, `(let ((x 1.0)) (decf x) x)`, Float(0.0))
	assertEqual(t, `(let ((x 1)) (decf x 2) x)`, Integer(-1))
	assertEqual(t, `(let ((x 1.0)) (decf x 2) x)`, Float(-1))
}

func TestMod(t *testing.T) {
	assertEqual(t, `(mod -5 3)`, Integer(1))
	assertEqual(t, `(mod 5 -3)`, Integer(-1))
	assertEqual(t, `(rem -5 3)`, Integer(-2))
	assertEqual(t, `(rem 5 -3)`, Integer(2))
}
