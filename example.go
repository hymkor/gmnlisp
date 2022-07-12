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
	value, err := lisp.Interpret("(+ 1 2)")
	if err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		return
	}

	value.PrintTo(os.Stdout)
	fmt.Println()
}
