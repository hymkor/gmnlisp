package gmnlisp

import (
	"context"
	"strconv"
	"strings"
)

func funStringAppend(ctx context.Context, w *World, list []Node) (Node, error) {
	if len(list) <= 0 {
		return Null, nil
	}
	if _, ok := list[0].(UTF32String); ok {
		length := 0
		for _, n := range list {
			str, ok := n.(UTF32String)
			if !ok {
				return nil, ErrExpectedString
			}
			length += len(str)
		}
		buffer := make([]Rune, 0, length)
		for _, n := range list {
			str := n.(UTF32String)
			buffer = append(buffer, []Rune(str)...)
		}
		return UTF32String(buffer), nil
	} else {
		var buffer strings.Builder
		for _, s := range list {
			str, ok := s.(UTF8String)
			if !ok {
				return nil, ErrExpectedString
			}
			str.PrintTo(&buffer, PRINC)
		}
		return UTF8String(buffer.String()), nil
	}
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
