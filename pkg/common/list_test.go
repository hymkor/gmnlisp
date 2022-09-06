package common

import (
	"testing"

	. "github.com/hymkor/gmnlisp"
)

func TestList(t *testing.T) {
	assertEqual(t, `(cadr '(1 2 3))`, Integer(2))
	assertEqual(t, `(caddr '(1 2 3 4 5 ))`, Integer(3))
	assertEqual(t, `(cadddr '(1 2 3 4 5))`, Integer(4))
	assertEqual(t, `(cddr '(1 2 3 4 5))`,
		List(Integer(3), Integer(4), Integer(5)))
	assertEqual(t, `(cdddr '(1 2 3 4 5))`,
		List(Integer(4), Integer(5)))

	assertEqual(t, `(nth 2 '(10 20 30 40))`, Integer(30))
	assertEqual(t, `(nthcdr 2 '(10 20 30 40))`, List(Integer(30), Integer(40)))
}

func TestFirst(t *testing.T) {
	// assertEqual(t, `(first '(1 2 3 4))`, Integer(1))
	assertEqual(t, `(second '(1 2 3 4))`, Integer(2))
	assertEqual(t, `(third '(1 2 3 4))`, Integer(3))
	assertEqual(t, `(rest '(1 2 3 4))`, List(Integer(2), Integer(3), Integer(4)))
}
