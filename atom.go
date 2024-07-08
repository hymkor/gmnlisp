package gmnlisp

import (
	"context"
	"fmt"
	"io"
	"unicode"
)

type _TrueType struct{}

var trueClass = registerNewBuiltInClass[_TrueType]("<truetype>")

func (t _TrueType) ClassOf() Class {
	return trueClass
}

func (_TrueType) PrintTo(w io.Writer, m PrintMode) (int, error) {
	return io.WriteString(w, "t")
}

func (t _TrueType) Eval(context.Context, *World) (Node, error) {
	return t, nil
}

var True Node = _TrueType{}

func (_TrueType) Equals(n Node, m EqlMode) bool {
	_, ok := n.(_TrueType)
	return ok
}

type _NullType struct{}

var nullClass = registerNewBuiltInClass[_NullType]("<null>")

func (_NullType) ClassOf() Class {
	return nullClass
}

func (_NullType) PrintTo(w io.Writer, m PrintMode) (int, error) {
	return io.WriteString(w, "nil")
}

func (nt _NullType) Eval(context.Context, *World) (Node, error) {
	return nt, nil
}

func (nt _NullType) Equals(n Node, m EqlMode) bool {
	if n == nil {
		return true
	}
	_, ok := n.(_NullType)
	return ok
}

var Null Node = _NullType{}

type idMap[T ~int] struct {
	id2name []string
	name2id map[string]T
}

func (idm *idMap[T]) NameToId(name string) T {
	if idm.name2id == nil {
		idm.name2id = make(map[string]T)
	}
	if id, ok := idm.name2id[name]; ok {
		return id
	}
	id := T(len(idm.name2id))
	idm.name2id[name] = id
	idm.id2name = append(idm.id2name, name)
	return id
}

func (idm *idMap[T]) Count() int {
	return len(idm.name2id)
}

type Symbol int

func (idm *idMap[T]) IdToName(id T) string {
	return idm.id2name[id]
}

var symbolManager = &idMap[Symbol]{}

func NewSymbol(s string) Symbol {
	return symbolManager.NameToId(s)
}

func genSym() Symbol {
	return NewSymbol(fmt.Sprintf("-gensym-%d-", symbolManager.Count()))
}

func funGensym(ctx context.Context, w *World) (Node, error) {
	return genSym(), nil
}

var symbolClass = registerNewBuiltInClass[Symbol]("<symbol>")

func (Symbol) ClassOf() Class {
	return symbolClass
}

func (s Symbol) PrintTo(w io.Writer, m PrintMode) (int, error) {
	return io.WriteString(w, symbolManager.IdToName(s))
}

func (s Symbol) Eval(_ context.Context, w *World) (Node, error) {
	return w.Get(s)
}

func (s Symbol) Equals(n Node, m EqlMode) bool {
	ns, ok := n.(Symbol)
	return ok && s == ns
}

func (s Symbol) String() string {
	return symbolManager.IdToName(s)
}

func (s Symbol) GoString() string {
	return symbolManager.IdToName(s)
}

type Rune rune

var characterClass = registerNewBuiltInClass[Rune]("<character>")

func (Rune) ClassOf() Class {
	return characterClass
}

func (r Rune) PrintTo(w io.Writer, m PrintMode) (int, error) {
	if m == PRINT {
		switch r {
		case '\t':
			return io.WriteString(w, `#\tab`)
		case '\n':
			return io.WriteString(w, `#\linefeed`)
		case '\r':
			return io.WriteString(w, `#\return`)
		case ' ':
			return io.WriteString(w, `#\space`)
		default:
			if unicode.IsLetter(rune(r)) {
				return fmt.Fprintf(w, `#\%c`, rune(r))
			} else {
				return fmt.Fprintf(w, `#\U%04X`, rune(r))
			}
		}
	} else {
		return fmt.Fprintf(w, "%c", rune(r))
	}
}

func (r Rune) Eval(_ context.Context, w *World) (Node, error) {
	return r, nil
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

func (k Keyword) PrintTo(w io.Writer, m PrintMode) (int, error) {
	return io.WriteString(w, keywordManager.IdToName(k))
}

func (k Keyword) Eval(context.Context, *World) (Node, error) {
	return k, nil
}

func (k Keyword) Equals(n Node, m EqlMode) bool {
	ks, ok := n.(Keyword)
	return ok && k == ks
}
