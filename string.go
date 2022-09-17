package gmnlisp

import (
	"context"
	"strconv"
	"strings"
	"unicode/utf8"
)

func funStringAppend(ctx context.Context, w *World, list []Node) (Node, error) {
	if len(list) <= 0 {
		return Null, nil
	}
	var buffer SeqBuilder
	if _, ok := list[0].(UTF32String); ok {
		buffer = &UTF32StringBuilder{}
	} else if _, ok := list[0].(UTF8String); ok {
		buffer = &UTF8StringBuilder{}
	} else {
		return nil, ErrNotSupportType
	}
	for _, s := range list {
		str, ok := s.(StringTypes)
		if !ok {
			return nil, ErrNotSupportType
		}
		str.EachRune(func(r Rune) error {
			buffer.Add(r)
			return nil
		})
	}
	return buffer.Sequence(), nil
}

func funParseInt(ctx context.Context, w *World, argv []Node) (Node, error) {
	// from CommonLisp
	s, ok := argv[0].(UTF32String)
	if !ok {
		return nil, ErrExpectedString
	}
	value, err := strconv.Atoi(string(s))
	if err != nil {
		return Null, nil
	}
	return Integer(value), nil
}

func compareString(argv []Node, f func(int) bool) (Node, error) {
	left, ok := argv[0].(StringTypes)
	if !ok {
		return nil, ErrExpectedString
	}
	right, ok := argv[1].(StringTypes)
	if !ok {
		return nil, ErrExpectedString
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
	_subStr, ok := argv[0].(StringTypes)
	if !ok {
		return nil, ErrExpectedString
	}
	subStr := _subStr.String()
	start := 0
	if len(argv) >= 3 {
		_start, ok := argv[2].(Integer)
		if !ok {
			return nil, ErrExpectedNumber
		}
		if len(argv) >= 4 {
			return nil, ErrTooManyArguments
		}
		start = int(_start)
	}
	_str, ok := argv[1].(StringTypes)
	if !ok {
		return nil, ErrExpectedString
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
