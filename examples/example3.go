//go:build example
// +build example

package main

import (
	"context"
	"fmt"
	"os"

	"github.com/hymkor/gmnlisp"
)

func mains(name string) error {
	code, err := os.ReadFile(name)
	if err != nil {
		return fmt.Errorf("%s: %w", name, err)
	}
	lisp := gmnlisp.New()
	lisp = lisp.Let(gmnlisp.Variables{
		gmnlisp.NewSymbol("$0"): gmnlisp.String(name)})
	_, err = lisp.InterpretBytes(context.Background(), code)
	if err != nil {
		return fmt.Errorf("%s: %w", name, err)
	}
	return err
}

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintf(os.Stderr, "Usage: %s SCRIPT\n", os.Args[0])
		os.Exit(1)
	}
	if err := mains(os.Args[1]); err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		os.Exit(1)
	}
}
