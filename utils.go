package gmnlisp

import (
	"context"
	"errors"
	"io"
	"strings"
)

var (
	ErrDevisionByZero       = errors.New("Devision by zeor")
	ErrExpectedCharacter    = errors.New("Expected character")
	ErrExpectedCons         = errors.New("Expected CONS")
	ErrExpectedFunction     = errors.New("expected function")
	ErrExpectedNumber       = errors.New("Expected number")
	ErrExpectedReader       = errors.New("Expected Reader")
	ErrExpectedSequence     = errors.New("Expected Sequence")
	ErrExpectedString       = errors.New("Expected string")
	ErrExpectedSymbol       = errors.New("Expected symbol")
	ErrExpectedSymbolOrList = errors.New("Expected symbol or list")
	ErrExpectedWriter       = errors.New("Expected Writer")
	ErrIndexOutOfRange      = errors.New("Index out of range")
	ErrInvalidFormat        = errors.New("Invalid format")
	ErrNotSupportBySetf     = errors.New("Not supported by setf")
	ErrNotSupportType       = errors.New("Not support type")
	ErrQuit                 = errors.New("Bye")
	ErrAbort                = errors.New("Abort")
	ErrTooFewArguments      = errors.New("Too few arguments")
	ErrTooManyArguments     = errors.New("Too many arguments")
	ErrTooShortTokens       = errors.New("too short tokens")
	ErrVariableUnbound      = errors.New("Unbound variable")
	ErrCanNotParseNumber    = errors.New("Can not parse number")
	ErrExpectedArray        = errors.New("Expected array")
)

type EqlMode int

const (
	STRICT EqlMode = iota
	EQUAL
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
	PrintTo(io.Writer, PrintMode) (int, error)
}

func ToString(node Node, m PrintMode) string {
	if node == nil {
		return "()"
	}
	var buffer strings.Builder
	node.PrintTo(&buffer, m)
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

func Shift(list Node) (Node, Node, error) {
	cons, ok := list.(*Cons)
	if !ok {
		return nil, nil, ErrTooFewArguments
	}
	return cons.GetCar(), cons.Cdr, nil
}

func ListToArray(list Node, slice []Node) error {
	for i := 0; i < len(slice); i++ {
		var err error

		slice[i], list, err = Shift(list)
		if err != nil {
			return err
		}
	}
	if HasValue(list) {
		return ErrTooManyArguments
	}
	return nil
}

func CheckContext(ctx context.Context) error {
	select {
	case <-ctx.Done():
		return ctx.Err()
	default:
		return nil
	}
}

func ignoreEOF(err error) error {
	if err == io.EOF {
		return nil
	}
	return err
}
