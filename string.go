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

func funStrCase(ctx context.Context, w *World, argv []Node) (Node, error) {
	// from autolisp
	if str, ok := argv[0].(UTF32String); ok {
		return UTF32String(strings.ToUpper(string(str))), nil
	} else if str, ok := argv[0].(UTF8String); ok {
		return UTF8String(strings.ToUpper(string(str))), nil
	}
	return nil, ErrExpectedString
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

func funToUTF8(ctx context.Context, w *World, argv []Node) (Node, error) {
	if s, ok := argv[0].(UTF8String); ok {
		return s, nil
	}
	if s, ok := argv[0].(UTF32String); ok {
		return UTF8String(s.String()), nil
	}
	return nil, ErrNotSupportType
}

func funToUTF32(ctx context.Context, w *World, argv []Node) (Node, error) {
	if s, ok := argv[0].(UTF8String); ok {
		return UTF32String(s.String()), nil
	}
	if s, ok := argv[0].(UTF32String); ok {
		return s, nil
	}
	return nil, ErrNotSupportType
}
