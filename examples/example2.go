//go:build example
// +build example

package main

import (
	"context"
	"fmt"
	"os"

	"github.com/hymkor/gmnlisp"
)

func mains() error {
	lisp := gmnlisp.New()
	ctx := context.TODO()
	f, err := lisp.Interpret(ctx, `(lambda (a b) (+ a b))`)
	if err != nil {
		return err
	}
	value, err := lisp.Call(
		ctx,
		f,
		gmnlisp.Float(1.0),
		gmnlisp.Float(2.0))
	if err != nil {
		return err
	}
	value.PrintTo(os.Stdout, gmnlisp.PRINT)
	fmt.Println()
	return nil
}

func main() {
	if err := mains(); err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		os.Exit(1)
	}
}
