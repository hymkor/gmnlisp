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

func TestEq(t *testing.T) {
	assertEqual(t, `(eq 1 1)`, True)
	assertEqual(t, `(eq 1 2)`, Null)
	assertEqual(t, `(eq (cons 1 2) (cons 1 2))`, Null)
	assertEqual(t, `(let ((a (cons 1 2))) (eq a a))`, True)

	assertEqual(t, `(eql 1 1)`, True)
	assertEqual(t, `(eql 1 2)`, Null)
	assertEqual(t, `(eql (cons 1 2) (cons 1 2))`, True)
	assertEqual(t, `(let ((a (cons 1 2))) (eql a a))`, True)

	assertEqual(t, `(eql 1 1)`, True)
	assertEqual(t, `(eql 1 1.0)`, Null)
	assertEqual(t, `(equal 1 1.0)`, True)
	assertEqual(t, `(equal "A" "A")`, True)
	assertEqual(t, `(equal "a" "A")`, Null)
	assertEqual(t, `(equalp "a" "A")`, True)
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

func TestReadFromString(t *testing.T) {
	assertEqual(t, `(read-from-string "(1 2 3)")`, List(Integer(1), Integer(2), Integer(3)))
}

func TestCreateStringInputStream(t *testing.T) {
	assertEqual(t, `
		(let ((fd (create-string-input-stream "1\n2\n3")))
			(read-line fd)
			(read-line fd)
		)`, UTF32String("2"))
}
