package gmnlisp

import (
	"errors"
	"fmt"
)

var (
	ErrTooFewOrTooManyArguments = errors.New("Too few or too many arguments")
	ErrExpectedCons             = errors.New("Expected CONS")
	ErrExpectedNumber           = errors.New("Expected a number")
	ErrExpectedSymbol           = errors.New("Expected symbol")
)

func ForEachQuote(this Node, f func(Node) error) error {
	if IsNull(this) {
		return nil
	}
	for {
		cons, ok := this.(*Cons)
		if !ok {
			return fmt.Errorf("%w (%s)", ErrExpectedCons, Node2String(this))
		}
		if err := f(cons.Car); err != nil {
			return err
		}
		if IsNull(cons.Cdr) {
			return nil
		}
		this = cons.Cdr
	}
}

func ForEachEval(this Node, f func(Node) error) error {
	return ForEachQuote(this, func(quotedOne Node) error {
		evalOne, err := quotedOne.Eval()
		if err != nil {
			return err
		}
		return f(evalOne)
	})
}

func ShiftAndEval(node Node) (Node, Node, error) {
	cons, ok := node.(*Cons)
	if !ok {
		return nil, nil, ErrTooFewOrTooManyArguments
	}
	value, err := cons.GetCar().Eval()
	if err != nil {
		return nil, nil, err
	}
	return value, cons.Cdr, nil
}

func CmdCons(node Node) (Node, error) {
	first, rest, err := ShiftAndEval(node)
	if err != nil {
		return nil, err
	}
	second, rest, err := ShiftAndEval(rest)
	if err != nil {
		return nil, err
	}
	if HasValue(rest) {
		return nil, ErrTooFewOrTooManyArguments
	}
	return &Cons{Car: first, Cdr: second}, err
}

func CmdCar(param Node) (Node, error) {
	first, _, err := ShiftAndEval(param)
	if err != nil {
		return nil, err
	}
	cons, ok := first.(*Cons)
	if !ok {
		return nil, ErrExpectedCons
	}
	return cons.Car, nil
}

func CmdCdr(param Node) (Node, error) {
	first, _, err := ShiftAndEval(param)
	if err != nil {
		return nil, err
	}
	cons, ok := first.(*Cons)
	if !ok {
		return nil, ErrExpectedCons
	}
	return cons.Cdr, nil
}

func CmdQuote(param Node) (Node, error) {
	cons, ok := param.(*Cons)
	if !ok {
		return nil, ErrTooFewOrTooManyArguments
	}
	return cons.Car, nil
}

func CmdAtom(param Node) (Node, error) {
	cons, ok := param.(*Cons)
	if !ok {
		return nil, ErrExpectedCons
	}
	if _, ok := cons.Car.(*Cons); ok {
		return NullValue, nil
	}
	return TrueValue, nil
}

func CmdEqual(param Node) (Node, error) {
	first, rest, err := ShiftAndEval(param)
	if err != nil {
		return nil, err
	}
	for !IsNull(rest) {
		var next Node

		next, rest, err = ShiftAndEval(rest)
		if err != nil {
			return nil, err
		}
		if !first.Equals(next) {
			return NullValue, nil
		}
	}
	return TrueValue, nil
}
