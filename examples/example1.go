//go:build example
// +build example

package main

import (
	"context"
	"fmt"
	"os"

	"github.com/hymkor/gmnlisp"
)

func main() {
	lisp := gmnlisp.New()
	lisp.DefineGlobal("a", gmnlisp.Integer(1))
	lisp.DefineGlobal("b", gmnlisp.Integer(2))
	value, err := lisp.Interpret(context.TODO(), "(+ a b)")
	if err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		return
	}
	value.PrintTo(os.Stdout, gmnlisp.PRINT)
	fmt.Println()
}
