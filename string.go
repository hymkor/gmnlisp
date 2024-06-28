package gmnlisp

import (
	"context"
	"fmt"
	"io"
	"strconv"
	"strings"
	"unicode/utf8"
)

type String string

var stringClass = registerNewBuiltInClass[String]("<string>")

func (String) ClassOf() Class {
	return stringClass
}

func (s String) String() string {
	return string(s)
}

func (s String) GoString() string {
	return strconv.Quote(string(s))
}

func (s String) PrintTo(w io.Writer, m PrintMode) (int, error) {
	if m == PRINC {
		return io.WriteString(w, string(s))
	} else {
		return fmt.Fprintf(w, "%#v", string(s))
	}
}

func (s String) Eval(context.Context, *World) (Node, error) {
	return s, nil
}

func (s String) Equals(n Node, m EqlMode) bool {
	ns, ok := n.(String)
	if !ok {
		_ns, ok := n.(String)
		if !ok {
			return false
		}
		if m == STRICT {
			return false
		}
		ns = String(string(_ns))
	}
	if m == EQUALP {
		return strings.EqualFold(string(s), string(ns))
	}
	return string(s) == string(ns)
}

func (s String) firstRuneAndRestString() (Rune, String, bool) {
	if len(s) <= 0 {
		return Rune(utf8.RuneError), "", false
	}
	r, siz := utf8.DecodeRuneInString(string(s))
	return Rune(r), String(s[siz:]), true
}

func (s String) EachRune(f func(Rune) error) error {
	for _, r := range s {
		if err := f(Rune(r)); err != nil {
			return err
		}
	}
	return nil
}

func (s String) FirstAndRest() (Node, Node, bool) {
	if len(s) <= 0 {
		return nil, Null, false
	}
	r, siz := utf8.DecodeRuneInString(string(s))
	return Rune(r), String(s[siz:]), true
}

func (s String) Add(n Node) (Node, error) {
	value, err := ExpectString(n)
	if err != nil {
		return nil, err
	}
	news := make([]byte, 0, len(s)+len(value)+1)
	news = append(news, s...)
	news = append(news, value...)
	return String(news), nil
}

func (s String) LessThan(n Node) (bool, error) {
	ns, err := ExpectString(n)
	if err != nil {
		return false, err
	}
	return string(s) < string(ns), nil
}

func funStringAppend(ctx context.Context, w *World, list []Node) (Node, error) {
	if len(list) <= 0 {
		return Null, nil
	}
	var buffer strings.Builder
	for _, s := range list {
		str, err := ExpectString(s)
		if err != nil {
			return nil, err
		}
		buffer.WriteString(str.String())
	}
	return String(buffer.String()), nil
}

func compareString(argv []Node, f func(int) bool) (Node, error) {
	left, err := ExpectString(argv[0])
	if err != nil {
		return nil, err
	}
	right, err := ExpectString(argv[1])
	if err != nil {
		return nil, err
	}
	cmp := strings.Compare(left.String(), right.String())
	if f(cmp) {
		return True, nil
	}
	return Null, nil
}

func funStringLt(ctx context.Context, w *World, argv []Node) (Node, error) {
	return compareString(argv, func(cmp int) bool { return cmp < 0 })
}
func funStringLe(ctx context.Context, w *World, argv []Node) (Node, error) {
	return compareString(argv, func(cmp int) bool { return cmp <= 0 })
}
func funStringEq(ctx context.Context, w *World, argv []Node) (Node, error) {
	return compareString(argv, func(cmp int) bool { return cmp == 0 })
}
func funStringGt(ctx context.Context, w *World, argv []Node) (Node, error) {
	return compareString(argv, func(cmp int) bool { return cmp > 0 })
}
func funStringGe(ctx context.Context, w *World, argv []Node) (Node, error) {
	return compareString(argv, func(cmp int) bool { return cmp >= 0 })
}
func funStringNe(ctx context.Context, w *World, argv []Node) (Node, error) {
	return compareString(argv, func(cmp int) bool { return cmp != 0 })
}

func funStringIndex(ctx context.Context, w *World, argv []Node) (Node, error) {
	if len(argv) < 2 {
		return nil, ErrTooFewArguments
	}
	_subStr, err := ExpectString(argv[0])
	if err != nil {
		return nil, err
	}
	subStr := _subStr.String()
	start := 0
	if len(argv) >= 3 {
		_start, err := ExpectInteger(argv[2])
		if err != nil {
			return nil, err
		}
		if len(argv) >= 4 {
			return nil, ErrTooManyArguments
		}
		start = int(_start)
	}
	_str, err := ExpectString(argv[1])
	if err != nil {
		return nil, err
	}
	str := _str.String()

	for i := 0; i < start && len(str) > 0; i++ {
		_, siz := utf8.DecodeRuneInString(str)
		str = str[siz:]
	}

	index := strings.Index(str, subStr)
	if index < 0 {
		return Null, nil
	}
	return Integer(start + utf8.RuneCountInString(str[:index])), nil
}

func funCreateString(ctx context.Context, w *World, list []Node) (Node, error) {
	if len(list) < 1 {
		return nil, ErrTooFewArguments
	}
	length, err := ExpectInteger(list[0])
	if err != nil {
		return nil, err
	}
	if len(list) > 2 {
		return nil, ErrTooManyArguments
	}
	ch := Rune(' ')
	if len(list) == 2 {
		ch, err = ExpectCharacter(list[1])
		if err != nil {
			return nil, err
		}
	}
	return String(strings.Repeat(string(ch), int(length))), nil
}
