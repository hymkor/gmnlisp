package eval

import (
	"context"
	"os"

	"github.com/hymkor/gmnlisp"
)

func init() {
	gmnlisp.Export(gmnlisp.NewSymbol("eval"), gmnlisp.Function1(funEval))
	gmnlisp.Export(gmnlisp.NewSymbol("load"), gmnlisp.Function1(funLoad))
}

func funEval(ctx context.Context, w *gmnlisp.World, arg gmnlisp.Node) (gmnlisp.Node, error) {
	return w.Eval(ctx, arg)
}

func funLoad(ctx context.Context, w *gmnlisp.World, arg gmnlisp.Node) (gmnlisp.Node, error) {
	fname, err := gmnlisp.ExpectClass[gmnlisp.String](ctx, w, arg)
	if err != nil {
		return nil, err
	}
	script, err := os.ReadFile(fname.String())
	if err != nil {
		return nil, err
	}
	return w.InterpretBytes(ctx, script)
}
