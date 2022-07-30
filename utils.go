package gmnlisp

import (
	"context"
	"errors"
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
	ErrExpectedSequence = errors.New("Expected Sequence")
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
	Eval(context.Context, *World) (Node, error)
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
	var cons Node = Null
	for i := len(nodes) - 1; i >= 0; i-- {
		cons = &Cons{
			Car: nodes[i],
			Cdr: cons,
		}
	}
	return cons
}

func shift(list Node) (Node, Node, error) {
	cons, ok := list.(*Cons)
	if !ok {
		return nil, nil, ErrTooFewArguments
	}
	return cons.GetCar(), cons.Cdr, nil
}

func listToArray(list Node, slice []Node) error {
	for i := 0; i < len(slice); i++ {
		var err error

		slice[i], list, err = shift(list)
		if err != nil {
			return err
		}
	}
	if HasValue(list) {
		return ErrTooManyArguments
	}
	return nil
}

func checkContext(ctx context.Context) error {
	select {
	case <-ctx.Done():
		return ctx.Err()
	default:
		return nil
	}
}
