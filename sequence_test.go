package gmnlisp

import (
	"testing"
)

func TestLength(t *testing.T) {
	assertEqual(t, `(length (list 1 2 3 4))`, Integer(4))
	assertEqual(t, `(length '(list 1 2 3))`, Integer(4))

	assertEqual(t, `(length "12345")`, Integer(5))
}

func TestMapCar(t *testing.T) {
	assertEqual(t, `(mapcar (function +) '(1 2 3) '(4 5 6))`,
		List(Integer(5), Integer(7), Integer(9)))
	assertEqual(t, `(mapcar #'+ '(1 2 3) '(4 5 6))`,
		List(Integer(5), Integer(7), Integer(9)))
	assertEqual(t, `(mapcar '+ '(1 2 3) '(4 5 6))`,
		List(Integer(5), Integer(7), Integer(9)))
	assertEqual(t, `(mapcar (lambda (a b) (+ a b)) '(1 2 3) '(4 5 6))`,
		List(Integer(5), Integer(7), Integer(9)))
	assertEqual(t, `(mapcar #'(lambda (a b) (+ a b)) '(1 2 3) '(4 5 6))`,
		List(Integer(5), Integer(7), Integer(9)))
}

func TestMap(t *testing.T) {
	assertEqual(t, `(map 'string '1+ "123")`, String("234"))
	assertEqual(t, `(map 'list '1+ '(1 2 3))`, List(Integer(2), Integer(3), Integer(4)))
	assertEqual(t, `(length (map 'list #'null '(nil 2 3)))`, Integer(3))
}

func TestCoerce(t *testing.T) {
	assertEqual(t, `(coerce '(#\a #\b) 'string)`, String("ab"))
	assertEqual(t, `(coerce '(#\a #\b) 'list)`, List(Rune('a'), Rune('b')))
}

func TestConcatenate(t *testing.T) {
	assertEqual(t, `(concatenate 'string "123" "456")`, String("123456"))
	assertEqual(t, `(concatenate 'list '(1 2 3) '(4 5 6))`,
		List(Integer(1), Integer(2), Integer(3), Integer(4), Integer(5), Integer(6)))
}

func TestReverse(t *testing.T) {
	assertEqual(t, `(reverse '(1 2 3 4))`,
		List(Integer(4), Integer(3), Integer(2), Integer(1)))
	assertEqual(t, `(reverse "12345")`, String("54321"))
}
