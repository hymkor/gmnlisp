package common

import (
	"testing"

	. "github.com/hymkor/gmnlisp"
)

func TestFind(t *testing.T) {
	assertEqual(t, `(find 2 '(1 2 3))`, Integer(2))
	assertEqual(t, `(find 2.0 '(1 2 3))`, Null)
	assertEqual(t, `(find 2.0 '(1 2 3) :test #'(lambda (a b) (equalp a b)))`, Integer(2))
	assertEqual(t, `(find 2.0 '(1 2 3) :test #'(lambda (a b) (eql a b)))`, Null)
}

func TestMember(t *testing.T) {
	assertEqual(t, `(member 'c '(a b c d e))`,
		List(NewSymbol("c"), NewSymbol("d"), NewSymbol("e")))
	assertEqual(t, `(member #\c "abcd")`, String("cd"))
	assertEqual(t, `(member #\C "abcd" :test #'(lambda (a b) (equalp a b)))`, String("cd"))
}

func TestPosition(t *testing.T) {
	assertEqual(t, `(position 'c '(a b c d e))`, Integer(2))
	assertEqual(t, `(position #\c "abcd")`, Integer(2))
	assertEqual(t, `(position #\C "abcd")`, Null)
	assertEqual(t, `(position #\C "abcd" :test #'(lambda (a b) (equalp a b)))`, Integer(2))
}

func TestConcatenate(t *testing.T) {
	assertEqual(t, `(concatenate 'string "123" "456")`, String("123456"))
	assertEqual(t, `(concatenate 'list '(1 2 3) '(4 5 6))`,
		List(Integer(1), Integer(2), Integer(3), Integer(4), Integer(5), Integer(6)))
	assertEqual(t, `(apply #'concatenate 'string '("1" "2" "3"))`, String("123"))
}
