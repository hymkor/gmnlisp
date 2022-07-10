package gmnlisp

import (
	"errors"
)

var (
	ErrTooFewOrTooManyArguments = errors.New("Too few or too many arguments")
	ErrExpectedCons             = errors.New("Expected CONS")
	ErrExpectedNumber           = errors.New("Expected a number")
	ErrExpectedSymbol           = errors.New("Expected symbol")
)

func cmdCons(ins *Instance, node Node) (Node, error) {
	first, rest, err := ins.ShiftAndEvalCar(node)
	if err != nil {
		return nil, err
	}
	second, rest, err := ins.ShiftAndEvalCar(rest)
	if err != nil {
		return nil, err
	}
	if HasValue(rest) {
		return nil, ErrTooFewOrTooManyArguments
	}
	return &Cons{Car: first, Cdr: second}, err
}

func cmdCar(ins *Instance, param Node) (Node, error) {
	first, _, err := ins.ShiftAndEvalCar(param)
	if err != nil {
		return nil, err
	}
	cons, ok := first.(*Cons)
	if !ok {
		return nil, ErrExpectedCons
	}
	return cons.Car, nil
}

func cmdCdr(ins *Instance, param Node) (Node, error) {
	first, _, err := ins.ShiftAndEvalCar(param)
	if err != nil {
		return nil, err
	}
	cons, ok := first.(*Cons)
	if !ok {
		return nil, ErrExpectedCons
	}
	return cons.Cdr, nil
}

func cmdQuote(_ *Instance, param Node) (Node, error) {
	cons, ok := param.(*Cons)
	if !ok {
		return nil, ErrTooFewOrTooManyArguments
	}
	return cons.Car, nil
}

func cmdAtom(_ *Instance, param Node) (Node, error) {
	cons, ok := param.(*Cons)
	if !ok {
		return nil, ErrExpectedCons
	}
	if _, ok := cons.Car.(*Cons); ok {
		return Null, nil
	}
	return TrueValue, nil
}

func cmdEqual(ins *Instance, param Node) (Node, error) {
	first, rest, err := ins.ShiftAndEvalCar(param)
	if err != nil {
		return nil, err
	}
	for HasValue(rest) {
		var next Node

		next, rest, err = ins.ShiftAndEvalCar(rest)
		if err != nil {
			return nil, err
		}
		if !first.Equals(next) {
			return Null, nil
		}
	}
	return TrueValue, nil
}
