package gmnlisp

import (
	"context"
	"errors"
	"strconv"
	"strings"
)

func funStrCat(ctx context.Context, w *World, list []Node) (Node, error) {
	// from autolisp
	var buffer strings.Builder
	for _, n := range list {
		str, ok := n.(String)
		if !ok {
			return nil, ErrExpectedString
		}
		buffer.WriteString(string(str))
	}
	return String(buffer.String()), nil
}

func funStrLen(ctx context.Context, w *World, argv []Node) (Node, error) {
	// from autolisp
	str, ok := argv[0].(String)
	if !ok {
		return nil, ErrExpectedString
	}
	return Integer(len(string(str))), nil
}

func funStrCase(ctx context.Context, w *World, argv []Node) (Node, error) {
	// from autolisp
	str, ok := argv[0].(String)
	if !ok {
		return nil, ErrExpectedString
	}
	return String(strings.ToUpper(string(str))), nil
}

func cmdSubStr(ctx context.Context, w *World, n Node) (Node, error) {
	// from autolisp
	str, n, err := w.shiftAndEvalCar(ctx, n)
	if err != nil {
		return nil, err
	}
	_str, ok := str.(String)
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
	return String(_str), nil
}

func funParseInt(ctx context.Context, w *World, argv []Node) (Node, error) {
	// from CommonLisp
	s, ok := argv[0].(String)
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
	s, ok := argv[0].(String)
	if !ok {
		return nil, ErrExpectedString
	}
	sep, ok := argv[1].(String)
	if !ok {
		return nil, ErrExpectedString
	}
	result := strings.Split(string(s), string(sep))
	var list Node = Null
	for i := len(result) - 1; i >= 0; i-- {
		list = &Cons{
			Car: String(result[i]),
			Cdr: list,
		}
	}
	return list, nil
}
