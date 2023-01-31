package gmnlisp

import (
	"context"
	"errors"
	"fmt"
	"io"
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
	ErrExpectedKeyword      = errors.New("Expected keyword")
	ErrExpectedWriter       = errors.New("Expected Writer")
	ErrIndexOutOfRange      = errors.New("Index out of range")
	ErrInvalidFormat        = errors.New("Invalid format")
	ErrNotSupportType       = errors.New("Not support type")
	ErrQuit                 = errors.New("Bye")
	ErrAbort                = errors.New("Abort")
	ErrTooFewArguments      = errors.New("Too few arguments")
	ErrTooManyArguments     = errors.New("Too many arguments")
	ErrTooShortTokens       = errors.New("too short tokens")
	ErrVariableUnbound      = errors.New("Unbound variable")
	ErrCanNotParseNumber    = errors.New("Can not parse number")
	ErrExpectedArray        = errors.New("Expected array")
	ErrExpectedMacro        = errors.New("Expected macro")
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
	String() string
	GoString() string
}

// IsNone returns whether `node` does not have a value or not
func IsNone(node Node) bool {
	if node == nil {
		return true
	}
	_, ok := node.(_NullType)
	return ok
}

// Deprecated: use IsNone
func IsNull(node Node) bool {
	return IsNone(node)
}

// IsSome returns whether `node` has a value or not
func IsSome(node Node) bool {
	if node == nil {
		return false
	}
	_, ok := node.(_NullType)
	return !ok
}

// Deprecated: use IsSome
func HasValue(node Node) bool {
	return IsSome(node)
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
	return cons.getCar(), cons.Cdr, nil
}

func ListToArray(list Node, slice []Node) error {
	for i := 0; i < len(slice); i++ {
		var err error

		slice[i], list, err = Shift(list)
		if err != nil {
			return err
		}
	}
	if IsSome(list) {
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

func ignoreEOF(err error) error {
	if err == io.EOF {
		return nil
	}
	return err
}

func MakeError(e error, s any) error {
	return fmt.Errorf("%w: %#v", e, s)
}
