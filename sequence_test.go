package gmnlisp

import (
	"testing"
)

func TestMapList(t *testing.T) {
	assertEqual(t, `(maplist #'append '(1 2 3 4) '(1 2) '(1 2 3))`,
		List(List(Integer(1), Integer(2), Integer(3), Integer(4),
			Integer(1), Integer(2), Integer(1), Integer(2), Integer(3)),
			List(Integer(2), Integer(3), Integer(4), Integer(2), Integer(2), Integer(3))))
}

func TestMapL(t *testing.T) {
	assertEqual(t, `
		(let ((k 0))
			(mapl
				(lambda (x)
					(setq k (+ k (if (member (car x) (cdr x)) 0 1)))
				)
				'(a b a c d b c)
			)
		k)`, Integer(4))
}

func TestMapCon(t *testing.T) {
	assertEqual(t, `
		(mapcon
			(lambda (x)
				(if (member (car x) (cdr x)) (list (car x)))
			)
			'(a b a c d b c b c)
		)`, List(NewSymbol("a"), NewSymbol("b"), NewSymbol("c"), NewSymbol("b"), NewSymbol("c")))
}

func TestReverse(t *testing.T) {
	assertEqual(t, `(reverse '(1 2 3 4))`,
		List(Integer(4), Integer(3), Integer(2), Integer(1)))
}

func TestNReverse(t *testing.T) {
	assertEqual(t, `(nreverse '(1 2 3 4))`,
		List(Integer(4), Integer(3), Integer(2), Integer(1)))
}

func TestSubSeq(t *testing.T) {
	assertEqual(t, `(subseq "12345" 2 4)`, String("34"))
	assertEqual(t, `(subseq '(1 2 3 4 5) 2 4)`, List(Integer(3), Integer(4)))
}

func TestSetfSubSeq(t *testing.T) {
	assertEqual(t, `
		(let ((m "12345"))
			(setf (subseq m 2 4) "xx")
			m)`, String("12xx5"))

	assertEqual(t, `
		(let ((m (list 1 2 3 4 5)))
			(setf (subseq m 2 4) (list 0 0))
			m)`, List(Integer(1), Integer(2), Integer(0), Integer(0), Integer(5)))
}

func TestElt(t *testing.T) {
	assertEqual(t, `(elt '(a b c) 2)`, NewSymbol("c"))
	// assertEqual(t, `(elt (vector 'a 'b 'c) 1)`,NewSymbol("b"))
	assertEqual(t, `(elt "abc" 0)`, Rune('a'))
}
