package gmnlisp

import (
	"errors"
	"strings"
)

func cmdStrCat(w *World, n Node) (Node, error) {
	var buffer strings.Builder
	for HasValue(n) {
		var s Node
		var err error

		s, n, err = w.shiftAndEvalCar(n)
		if err != nil {
			return nil, err
		}
		str, ok := s.(String)
		if !ok {
			return nil, ErrExpectedString
		}
		buffer.WriteString(string(str))
	}
	return String(buffer.String()), nil
}

func cmdStrLen(w *World, n Node) (Node, error) {
	var argv [1]Node
	if err := w.evalListAll(n, argv[:]); err != nil {
		return nil, err
	}
	str, ok := argv[0].(String)
	if !ok {
		return nil, ErrExpectedString
	}
	return Integer(len(string(str))), nil
}

func cmdStrCase(w *World, n Node) (Node, error) {
	var argv [1]Node
	if err := w.evalListAll(n, argv[:]); err != nil {
		return nil, err
	}
	str, ok := argv[0].(String)
	if !ok {
		return nil, ErrExpectedString
	}
	return String(strings.ToUpper(string(str))), nil
}

func cmdSubStr(w *World, n Node) (Node, error) {
	str, n, err := w.shiftAndEvalCar(n)
	if err != nil {
		return nil, err
	}
	_str, ok := str.(String)
	if !ok {
		return nil, ErrExpectedString
	}

	var pos Node
	pos, n, err = w.shiftAndEvalCar(n)
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

		leng, n, err = w.shiftAndEvalCar(n)
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
