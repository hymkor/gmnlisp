package exit

import (
	"context"
	"fmt"

	"github.com/hymkor/gmnlisp"
)

type ExitError struct {
	Value int
}

func (e ExitError) IsControlFlow() {}

func (e ExitError) Error() string {
	return fmt.Sprintf("exit: %d", e.Value)
}

type AbortError struct{}

func (e AbortError) IsControlFlow() {}

func (e AbortError) Error() string {
	return "abort"
}

var (
	ErrAbort = AbortError{}
	ErrQuit  = ExitError{Value: 0}
)

func init() {
	gmnlisp.Export(gmnlisp.NewSymbol("abort"), gmnlisp.Function0(funAbort))
	gmnlisp.Export(gmnlisp.NewSymbol("quit"), &gmnlisp.Function{F: funQuit, Max: 1})
}

func funQuit(ctx context.Context, w *gmnlisp.World, args []gmnlisp.Node) (gmnlisp.Node, error) {
	if len(args) <= 0 {
		return gmnlisp.Null, ErrQuit
	}
	v, err := gmnlisp.ExpectClass[gmnlisp.Integer](ctx, w, args[0])
	if err != nil {
		return nil, err
	}
	if v < 0 {
		return nil, &gmnlisp.DomainError{
			Object: args[0],
			Reason: "not a non negative integer",
		}
	}
	if v > 255 {
		return nil, gmnlisp.ErrIndexOutOfRange
	}
	return gmnlisp.Null, ExitError{Value: int(v)}
}

func funAbort(context.Context, *gmnlisp.World) (gmnlisp.Node, error) {
	return gmnlisp.Null, ErrAbort
}
