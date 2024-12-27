package gmnlisp

import (
	"context"
	"testing"
)

func TestConvert(t *testing.T) {
	ctx := context.Background()
	w := New()
	assertEqual(t, `(convert 1 <character>)`, Rune(1))
	assertEqual(t, `(convert #\A <integer>)`, Integer('A'))
	assertEqual(t, `(convert 1 <float>)`, Float(1.0))
	assertEqual(t, `(convert 1 <string>)`, String(`1`))
	assertEqual(t, `(convert "1" <integer>)`, Integer(1))
	assertEqual(t, `(convert "1.1" <float>)`, Float(1.1))
	assertEqual(t, `(convert "s" <symbol>)`, NewSymbol("s"))
	assertEqual(t, `(convert #\a <integer>)`, Integer('a'))
	assertEqual(t, `(convert "abc" <list>)`, List(Rune('a'), Rune('b'), Rune('c')))
	assertEqual(t, `(convert '(1 2 3) <list>)`, List(Integer(1), Integer(2), Integer(3)))
	assertEqual(t, `(convert "abc" <general-vector>)`,
		NewVector(ctx, w, Rune('a'), Rune('b'), Rune('c')))
	assertEqual(t, `(convert '(1 2 3) <general-vector>)`,
		NewVector(ctx, w, Integer(1), Integer(2), Integer(3)))
	assertEqual(t, `(convert #(1 2 3) <list>)`,
		List(Integer(1), Integer(2), Integer(3)))
}
