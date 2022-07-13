//go:build example
// +build example

package main

import (
	"fmt"
	"os"

	"github.com/hymkor/gmnlisp"
)

func main() {
	lisp := gmnlisp.New()
	lisp.Set("a", gmnlisp.Integer(1))
	lisp.Set("b", gmnlisp.Integer(2))
	value, err := lisp.Interpret("(+ a b)")
	if err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		return
	}
	value.PrintTo(os.Stdout)
	fmt.Println()
}
