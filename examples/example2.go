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
	a, err := gmnlisp.ExpectClass[gmnlisp.Integer](ctx, w, args[0])
	if err != nil {
		return nil, err
	}
	b, err := gmnlisp.ExpectClass[gmnlisp.Integer](ctx, w, args[1])
	if err != nil {
		return nil, err
	}
	return a + b, nil
}

func main() {
	lisp := gmnlisp.New()
	lisp = lisp.Flet(
		gmnlisp.Functions{
			gmnlisp.NewSymbol("sum"): &gmnlisp.Function{C: 2, F: sum},
		})

	result, err := lisp.Interpret(context.Background(), `(sum 1 2)`)
	if err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		return
	}
	fmt.Printf("(sum 1 2)=%v\n", result)
}
