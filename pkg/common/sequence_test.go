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

func TestCoerce(t *testing.T) {
	assertEqual(t, `(coerce '(#\a #\b) 'string)`, String("ab"))
	assertEqual(t, `(coerce '(#\a #\b) 'list)`, List(Rune('a'), Rune('b')))
}

func TestMap(t *testing.T) {
	assertEqual(t, `(map 'string '1+ "123")`, String("234"))
	assertEqual(t, `(map 'list '1+ '(1 2 3))`, List(Integer(2), Integer(3), Integer(4)))
	assertEqual(t, `(length (map 'list #'null '(nil 2 3)))`, Integer(3))
}

func TestTypep(t *testing.T) {
	assertEqual(t, `(typep 1 'number)`, True)
	assertEqual(t, `(typep 1 'integer)`, True)
	assertEqual(t, `(typep 1.0 'float)`, True)
	assertEqual(t, `(typep 1 'float)`, Null)
	assertEqual(t, `(typep "ABC" 'string)`, True)
	assertEqual(t, `(typep 1 'string)`, Null)
}
