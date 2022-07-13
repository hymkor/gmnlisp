//go:build example
// +build example

package main

import (
	"fmt"
	"os"

	"github.com/hymkor/gmnlisp"
)

func mains() error {
	lisp := gmnlisp.New()
	f, err := lisp.Interpret(`(lambda (a b) (+ a b))`)
	if err != nil {
		return err
	}
	value, err := lisp.Call(
		f,
		gmnlisp.Float(1.0),
		gmnlisp.Float(2.0))
	if err != nil {
		return err
	}
	value.PrintTo(os.Stdout)
	fmt.Println()
	return nil
}

func main() {
	if err := mains(); err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		os.Exit(1)
	}
}
