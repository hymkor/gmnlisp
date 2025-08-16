package exit

import (
	"context"
	"errors"

	"github.com/hymkor/gmnlisp"
)

var (
	ErrAbort = errors.New("abort")
	ErrQuit  = errors.New("bye")
)

func init() {
	gmnlisp.Export(gmnlisp.NewSymbol("quit"), gmnlisp.Function0(funQuit))
	gmnlisp.Export(gmnlisp.NewSymbol("abort"), gmnlisp.Function0(funAbort))
}

func funQuit(context.Context, *gmnlisp.World) (gmnlisp.Node, error) {
	return gmnlisp.Null, ErrQuit
}

func funAbort(context.Context, *gmnlisp.World) (gmnlisp.Node, error) {
	return gmnlisp.Null, ErrAbort
}
