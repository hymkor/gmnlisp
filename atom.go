package gmnlisp

import (
	"context"
	"fmt"
	"unicode"
)

type trueType struct{}

var trueClass = registerNewBuiltInClass[trueType]("<truetype>", symbolClass)

func (t trueType) ClassOf() Class {
	return trueClass
}

func (trueType) String() string {
	return "T"
}

func (trueType) OriginalString() string {
	return "T"
}

var True Node = trueType{}

func (trueType) Equals(n Node, m EqlMode) bool {
	_, ok := n.(trueType)
	return ok
}

func (nt trueType) Id() int {
	return NewReserved("T").Id()
}

type nullType struct{}

var nullClass = registerNewBuiltInClass[nullType]("<null>", symbolClass, listClass)

func (nullType) ClassOf() Class {
	return nullClass
}

func (nullType) String() string {
	return "NIL"
}

func (nullType) OriginalString() string {
	return "NIL"
}

func (nt nullType) Equals(n Node, m EqlMode) bool {
	if n == nil {
		return true
	}
	_, ok := n.(nullType)
	return ok
}

func (nt nullType) Id() int {
	return NewReserved("NIL").Id()
}

var Null Node = nullType{}

type Rune rune

var characterClass = registerNewBuiltInClass[Rune]("<character>")

func (Rune) ClassOf() Class {
	return characterClass
}

func (r Rune) GoString() string {
	switch r {
	case '\t':
		return `#\tab`
	case '\n':
		return `#\linefeed`
	case '\r':
		return `#\return`
	case ' ':
		return `#\space`
	default:
		if unicode.IsLetter(rune(r)) {
			return fmt.Sprintf(`#\%c`, rune(r))
		} else {
			return fmt.Sprintf(`#\U%04X`, rune(r))
		}
	}
}

func (r Rune) String() string {
	return string(rune(r))
}

func (r Rune) Equals(n Node, m EqlMode) bool {
	if value, ok := n.(Rune); ok {
		if r == value {
			return true
		}
		if m == EQUALP {
			return unicode.ToLower(rune(r)) == unicode.ToLower(rune(value))
		}
		return false
	}
	if m == STRICT {
		return false
	}
	return false
}

func (r Rune) Add(ctx context.Context, w *World, n Node) (Node, error) {
	value, err := ExpectClass[Rune](ctx, w, n)
	if err == nil {
		return r + value, nil
	}
	if value, ok := n.(Integer); ok {
		return r + Rune(value), nil
	}
	return nil, err
}

func (r Rune) Sub(ctx context.Context, w *World, n Node) (Node, error) {
	value, err := ExpectClass[Rune](ctx, w, n)
	if err == nil {
		return r - value, nil
	}
	if value, ok := n.(Integer); ok {
		return r - Rune(value), nil
	}
	return nil, err
}

func compareRune(ctx context.Context, w *World, argv []Node, f func(rune) bool) (Node, error) {
	left, err := ExpectClass[Rune](ctx, w, argv[0])
	if err != nil {
		return nil, err
	}
	right, err := ExpectClass[Rune](ctx, w, argv[1])
	if err != nil {
		return nil, err
	}
	cmp := rune(left) - rune(right)
	if f(cmp) {
		return True, nil
	}
	return Null, nil
}

func funRuneLt(ctx context.Context, w *World, argv []Node) (Node, error) {
	return compareRune(ctx, w, argv, func(cmp rune) bool { return cmp < 0 })
}
func funRuneLe(ctx context.Context, w *World, argv []Node) (Node, error) {
	return compareRune(ctx, w, argv, func(cmp rune) bool { return cmp <= 0 })
}
func funRuneEq(ctx context.Context, w *World, argv []Node) (Node, error) {
	return compareRune(ctx, w, argv, func(cmp rune) bool { return cmp == 0 })
}
func funRuneGt(ctx context.Context, w *World, argv []Node) (Node, error) {
	return compareRune(ctx, w, argv, func(cmp rune) bool { return cmp > 0 })
}
func funRuneGe(ctx context.Context, w *World, argv []Node) (Node, error) {
	return compareRune(ctx, w, argv, func(cmp rune) bool { return cmp >= 0 })
}
func funRuneNe(ctx context.Context, w *World, argv []Node) (Node, error) {
	return compareRune(ctx, w, argv, func(cmp rune) bool { return cmp != 0 })
}
func funRuneIndex(ctx context.Context, w *World, argv []Node) (Node, error) {
	_char, err := ExpectClass[Rune](ctx, w, argv[0])
	if err != nil {
		return nil, err
	}
	char := rune(_char)
	str, err := ExpectClass[String](ctx, w, argv[1])
	if err != nil {
		return nil, err
	}
	var start int = 0
	if len(argv) >= 3 {
		_start, err := ExpectClass[Integer](ctx, w, argv[2])
		if err != nil {
			return nil, err
		}
		start = int(_start)
	}
	i := 0
	for _, c := range string(str) {
		if i >= start && c == char {
			return Integer(i), nil
		}
		i++
	}
	return Null, nil
}

type Keyword int

var keywordManager = idMap[Keyword]{}

func NewKeyword(name string) Keyword {
	return keywordManager.NameToId(name)
}

var keywordClass = registerNewBuiltInClass[Keyword]("<keyword>")

func (Keyword) ClassOf() Class {
	return keywordClass
}

func (k Keyword) String() string {
	return keywordManager.IdToName(k)[0]
}

func (k Keyword) Equals(n Node, m EqlMode) bool {
	ks, ok := n.(Keyword)
	return ok && k == ks
}
