//go:build example
// +build example

package main

import (
	"context"
	"fmt"
	"os"

	"github.com/hymkor/gmnlisp"
)

func sum(ctx context.Context, w *gmnlisp.World, args []gmnlisp.Node) (gmnlisp.Node, error) {
	a, ok := args[0].(gmnlisp.Integer)
	if !ok {
		return nil, fmt.Errorf("expect integer: %#v", args[0])
	}
	b, ok := args[1].(gmnlisp.Integer)
	if !ok {
		return nil, fmt.Errorf("expect integer: %#v", args[1])
	}
	return gmnlisp.Integer(a + b), nil
}

func main() {
	lisp := gmnlisp.New()
	lisp = lisp.Let(
		gmnlisp.Variables{
			gmnlisp.NewSymbol("sum"): &gmnlisp.Function{C: 2, F: sum},
		})

	result, err := lisp.Interpret(context.Background(), `(sum 1 2)`)
	if err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		return
	}
	fmt.Printf("(sum 1 2)=%v\n", result)
}
