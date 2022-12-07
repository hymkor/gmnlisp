package gmnlisp

import (
	"context"
	"regexp"
	"strings"
	"unicode/utf8"
)

func funStringAppend(ctx context.Context, w *World, list []Node) (Node, error) {
	if len(list) <= 0 {
		return Null, nil
	}
	var buffer strings.Builder
	for _, s := range list {
		str, ok := s.(String)
		if !ok {
			return nil, MakeError(ErrExpectedString, s)
		}
		buffer.WriteString(str.String())
	}
	return String(buffer.String()), nil
}

func compareString(argv []Node, f func(int) bool) (Node, error) {
	left, ok := argv[0].(String)
	if !ok {
		return nil, MakeError(ErrExpectedString, argv[0])
	}
	right, ok := argv[1].(String)
	if !ok {
		return nil, MakeError(ErrExpectedString, argv[1])
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
	_subStr, ok := argv[0].(String)
	if !ok {
		return nil, ErrExpectedString
	}
	subStr := _subStr.String()
	start := 0
	if len(argv) >= 3 {
		_start, ok := argv[2].(Integer)
		if !ok {
			return nil, MakeError(ErrExpectedNumber, argv[2])
		}
		if len(argv) >= 4 {
			return nil, ErrTooManyArguments
		}
		start = int(_start)
	}
	_str, ok := argv[1].(String)
	if !ok {
		return nil, MakeError(ErrExpectedString, argv[1])
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
	length, ok := list[0].(Integer)
	if !ok {
		return nil, MakeError(ErrExpectedNumber, list[0])
	}
	if len(list) > 2 {
		return nil, ErrTooManyArguments
	}
	ch := Rune(' ')
	if len(list) == 2 {
		var ok bool
		ch, ok = list[1].(Rune)
		if !ok {
			return nil, MakeError(ErrExpectedCharacter, list[1])
		}
	}
	return String(strings.Repeat(string(ch), int(length))), nil
}

var regexpCache = map[string]*regexp.Regexp{}

func getRegexpParam(list []Node) (*regexp.Regexp, string, error) {
	_pattern, ok := list[0].(String)
	if !ok {
		return nil, "", MakeError(ErrExpectedString, list[0])
	}
	pattern := _pattern.String()
	reg, ok := regexpCache[pattern]
	if !ok {
		var err error
		reg, err = regexp.Compile(pattern)
		if err != nil {
			return nil, "", MakeError(err, pattern)
		}
	}
	str, ok := list[1].(String)
	if !ok {
		return nil, "", MakeError(ErrExpectedString, list[1])
	}
	return reg, str.String(), nil
}

func funFindAllStringSubmatch(ctx context.Context, w *World, list []Node) (Node, error) {
	reg, str, err := getRegexpParam(list)
	if err != nil {
		return nil, err
	}
	m := reg.FindAllStringSubmatch(str, -1)
	if m == nil {
		return Null, nil
	}
	var cons Node = Null
	for i := len(m) - 1; i >= 0; i-- {
		var sub Node = Null
		for j := len(m[i]) - 1; j >= 0; j-- {
			sub = &Cons{
				Car: String(m[i][j]),
				Cdr: sub,
			}
		}
		cons = &Cons{
			Car: sub,
			Cdr: cons,
		}
	}
	return cons, nil
}

func funFindAllStringSubmatchIndex(ctx context.Context, w *World, list []Node) (Node, error) {
	reg, str, err := getRegexpParam(list)
	if err != nil {
		return nil, err
	}
	m := reg.FindAllStringSubmatchIndex(str, -1)
	if m == nil {
		return Null, nil
	}
	var cons Node = Null
	for i := len(m) - 1; i >= 0; i-- {
		var sub Node = Null
		for j := len(m[i]) - 1; j >= 0; j-- {
			sub = &Cons{
				Car: Integer(m[i][j]),
				Cdr: sub,
			}
		}
		cons = &Cons{
			Car: sub,
			Cdr: cons,
		}
	}
	return cons, nil
}
