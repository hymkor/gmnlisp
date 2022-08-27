package gmnlisp

import (
	"context"
	"errors"
	"strconv"
	"strings"
)

func funStringAppend(ctx context.Context, w *World, list []Node) (Node, error) {
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
}

func funStrCase(ctx context.Context, w *World, argv []Node) (Node, error) {
	// from autolisp
	str, ok := argv[0].(UTF32String)
	if !ok {
		return nil, ErrExpectedString
	}
	return UTF32String(strings.ToUpper(string(str))), nil
}

func cmdSubStr(ctx context.Context, w *World, n Node) (Node, error) {
	// from autolisp
	str, n, err := w.shiftAndEvalCar(ctx, n)
	if err != nil {
		return nil, err
	}
	_str, ok := str.(UTF32String)
	if !ok {
		return nil, ErrExpectedString
	}

	var pos Node
	pos, n, err = w.shiftAndEvalCar(ctx, n)
	if err != nil {
		return nil, err
	}
	_pos, ok := pos.(Integer)
	if !ok {
		return nil, ErrExpectedNumber
	}
	if _pos < 1 {
		return nil, errors.New("position has to be more than 0")
	}
	if int(_pos) > len(_str) {
		return nil, errors.New("position is bigger than string length")
	}

	_str = _str[int(_pos)-1:]
	if HasValue(n) {
		var leng Node

		leng, n, err = w.shiftAndEvalCar(ctx, n)
		if err != nil {
			return nil, err
		}
		if HasValue(n) {
			return nil, ErrTooManyArguments
		}
		_leng, ok := leng.(Integer)
		if !ok {
			return nil, ErrExpectedNumber
		}
		if _leng < 0 {
			return nil, errors.New("length has to be 0 or more than 0")
		}
		if int(_leng) > len(_str) {
			return nil, errors.New("length is too large")
		}
		_str = _str[:int(_leng)]
	}
	return UTF32String(_str), nil
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

func funSplitString(ctx context.Context, w *World, argv []Node) (Node, error) {
	// from emacs-lisp
	s, ok := argv[0].(UTF32String)
	if !ok {
		return nil, ErrExpectedString
	}
	sep, ok := argv[1].(UTF32String)
	if !ok {
		return nil, ErrExpectedString
	}
	result := strings.Split(string(s), string(sep))
	var list Node = Null
	for i := len(result) - 1; i >= 0; i-- {
		list = &Cons{
			Car: UTF32String(result[i]),
			Cdr: list,
		}
	}
	return list, nil
}
