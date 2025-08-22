package gmnlisp

import (
	"context"
	"errors"
	"fmt"
	"io"

	"github.com/hymkor/gmnlisp/parser"
)

var (
	ErrExpectedClass  = errors.New("expected class")
	ErrInvalidFormat  = errors.New("invalid format")
	ErrNoMatchMethods = errors.New("no match methods")
	ErrNotSupportType = errors.New("not support type")

	ErrTooFewArguments  = ProgramError{err: parser.ErrTooFewArguments}
	ErrTooManyArguments = ProgramError{err: parser.ErrTooManyArguments}
	ErrTooShortTokens   = parser.ErrTooShortTokens
	ErrIndexOutOfRange  = ProgramError{err: errors.New("index out of range")}
	ErrDotEnditList     = ProgramError{err: errors.New("dot-ended-list is illegal")}
)

var ExhaustThreshold int64 = 123456789

// IsNone returns whether `node` does not have a value or not
func IsNone(node Node) bool {
	if node == nil {
		return true
	}
	_, ok := node.(nullType)
	return ok
}

// IsSome returns whether `node` has a value or not
func IsSome(node Node) bool {
	if node == nil {
		return false
	}
	_, ok := node.(nullType)
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
