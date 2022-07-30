package gmnlisp

import (
	"testing"
)

func TestCmdGo(t *testing.T) {
	assertEqual(t, "(quote (1 . 2))", &Cons{Car: Integer(1), Cdr: Integer(2)})
	assertEqual(t, "(atom 1)", True)
	assertEqual(t, "(atom '(1 2))", Null)
	assertEqual(t, `(equal (list 1 (+ 1 1) (+ 1 2)) '(1 2 3))`, True)
}

func TestNotEqual(t *testing.T) {
	assertEqual(t, `(not (/= 1 1))`, True)
	assertEqual(t, `(not (= 1 1))`, Null)
	assertEqual(t, `(/= 1 2)`, True)
	assertEqual(t, `(/= 1 1)`, Null)
}

func TestZerop(t *testing.T) {
	assertEqual(t, `(zerop 0)`, True)
	assertEqual(t, `(zerop 0.0)`, True)
	assertEqual(t, `(zerop 1)`, Null)
	assertEqual(t, `(zerop 0.1)`, Null)
	assertEqual(t, `(zerop "")`, Null)
}

func TestNumberp(t *testing.T) {
	assertEqual(t, `(numberp 1)`, True)
	assertEqual(t, `(numberp 1.0)`, True)
	assertEqual(t, `(numberp "")`, Null)
}

func TestPlusp(t *testing.T) {
	assertEqual(t, `(plusp 1)`, True)
	assertEqual(t, `(plusp 1.0)`, True)
	assertEqual(t, `(plusp -1)`, Null)
	assertEqual(t, `(plusp -1.0)`, Null)
	assertEqual(t, `(plusp "")`, Null)
}

func TestMinusp(t *testing.T) {
	assertEqual(t, `(minusp 1)`, Null)
	assertEqual(t, `(minusp 1.0)`, Null)
	assertEqual(t, `(minusp -1)`, True)
	assertEqual(t, `(minusp -1.0)`, True)
	assertEqual(t, `(minusp "")`, Null)
}

func TestNull(t *testing.T) {
	assertEqual(t, `(null 1)`, Null)
	assertEqual(t, `(null "")`, Null)
	assertEqual(t, `(null nil)`, True)
}

func TestOddp(t *testing.T) {
	assertEqual(t, `(oddp 1)`, True)
	assertEqual(t, `(oddp 0)`, Null)
}

func TestEvenp(t *testing.T) {
	assertEqual(t, `(evenp 0)`, True)
	assertEqual(t, `(evenp 1)`, Null)
}

func TestAnyTypes(t *testing.T) {
	assertEqual(t, `(integerp 1)`, True)
	assertEqual(t, `(integerp "")`, Null)
	assertEqual(t, `(floatp 1.1)`, True)
	assertEqual(t, `(floatp 1)`, Null)
	assertEqual(t, `(symbolp 'a)`, True)
	assertEqual(t, `(symbolp 1)`, Null)
	assertEqual(t, `(stringp "1")`, True)
	assertEqual(t, `(stringp 1)`, Null)
	assertEqual(t, `(consp '(1) )`, True)
	assertEqual(t, `(consp 1)`, Null)
}

func TestTypep(t *testing.T) {
	assertEqual(t, `(typep 1 'number)`, True)
	assertEqual(t, `(typep 1 'integer)`, True)
	assertEqual(t, `(typep 1.0 'float)`, True)
	assertEqual(t, `(typep 1 'float)`, Null)
	assertEqual(t, `(typep "ABC" 'string)`, True)
	assertEqual(t, `(typep 1 'string)`, Null)
}

func TestAref(t *testing.T) {
	assertEqual(t, `(aref "1234" 2)`, Rune('3'))
	assertEqual(t, `(aref '(1 2 3 4) 3)`, Integer(4))
}
