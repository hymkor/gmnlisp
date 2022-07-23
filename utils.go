package gmnlisp

import (
	"errors"
	"fmt"
	"io"
	"strings"
)

var (
	ErrDevisionByZero   = errors.New("Devision by zeor")
	ErrExpectedCons     = errors.New("Expected CONS")
	ErrExpectedFunction = errors.New("expected function")
	ErrExpectedNumber   = errors.New("Expected number")
	ErrExpectedString   = errors.New("Expected string")
	ErrExpectedSymbol   = errors.New("Expected symbol")
	ErrExpectedWriter   = errors.New("Expected Writer")
	ErrNotSupportType   = errors.New("Not support type")
	ErrQuit             = errors.New("Bye")
	ErrTooFewArguments  = errors.New("Too few arguments")
	ErrTooManyArguments = errors.New("Too many arguments")
	ErrTooShortTokens   = errors.New("too short tokens")
	ErrVariableUnbound  = errors.New("Unbound variable")
)

type EqlMode int

const (
	EQUAL EqlMode = iota
	EQUALP
)

type PrintMode int

const (
	PRINT PrintMode = iota
	PRINC
)

type Node interface {
	Eval(*World) (Node, error)
	Equals(Node, EqlMode) bool
	PrintTo(io.Writer, PrintMode)
}

func toString(node Node) string {
	if node == nil {
		return "()"
	}
	var buffer strings.Builder
	node.PrintTo(&buffer, PRINT)
	return buffer.String()
}

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

func forEachList(list Node, f func(Node) error) error {
	for HasValue(list) {
		cons, ok := list.(*Cons)
		if !ok {
			return fmt.Errorf("%w (%s)", ErrExpectedCons, toString(list))
		}
		if err := f(cons.Car); err != nil {
			return err
		}
		list = cons.Cdr
	}
	return nil
}

func listToArray(list Node, slice []Node) error {
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

func listToSlice(list Node) ([]Node, error) {
	slice := []Node{}
	for HasValue(list) {
		cons, ok := list.(*Cons)
		if !ok {
			return nil, ErrExpectedCons
		}
		slice = append(slice, cons.Car)
		list = cons.Cdr
	}
	return slice, nil
}

func shiftList(list Node, count int) (Node, error) {
	for i := 0; i < count; i++ {
		if IsNull(list) {
			return Null, nil
		}
		cons, ok := list.(*Cons)
		if !ok {
			return nil, ErrExpectedCons
		}
		list = cons.Cdr
	}
	return list, nil
}
