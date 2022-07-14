package gmnlisp

import (
	"errors"
	"fmt"
)

var (
	ErrDevisionByZero   = errors.New("Devision by zeor")
	ErrExpectedCons     = errors.New("Expected CONS")
	ErrExpectedFunction = errors.New("expected function")
	ErrExpectedNumber   = errors.New("Expected number")
	ErrExpectedSymbol   = errors.New("Expected symbol")
	ErrTooManyArguments = errors.New("Too many arguments")
	ErrTooFewArguments  = errors.New("Too few arguments")
	ErrTooShortTokens   = errors.New("too short tokens")
	ErrVariableUnbound  = errors.New("Unbound variable")
)

func IsNull(node Node) bool {
	if node == nil {
		return true
	}
	_, ok := node.(_NullType)
	return ok
}

func HasValue(node Node) bool {
	if node == nil {
		return false
	}
	_, ok := node.(_NullType)
	return !ok
}

func List(nodes ...Node) Node {
	first := &Cons{Cdr: Null}
	last := first
	for len(nodes) > 0 {
		tmp := &Cons{
			Car: nodes[0],
			Cdr: Null,
		}
		last.Cdr = tmp
		last = tmp
		nodes = nodes[1:]
	}
	return first.Cdr
}

func forEachWithoutEval(this Node, f func(Node) error) error {
	for HasValue(this) {
		cons, ok := this.(*Cons)
		if !ok {
			return fmt.Errorf("%w (%s)", ErrExpectedCons, toString(this))
		}
		if err := f(cons.Car); err != nil {
			return err
		}
		this = cons.Cdr
	}
	return nil
}

func listToSlice(list Node, slice []Node) error {
	for i := 0; i < len(slice); i++ {
		cons, ok := list.(*Cons)
		if !ok {
			return ErrTooFewArguments
		}
		slice[i] = cons.Car
		list = cons.Cdr
	}
	if HasValue(list) {
		return ErrTooManyArguments
	}
	return nil
}
