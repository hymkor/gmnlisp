package main

import (
	"flag"
	"fmt"
	"os"

	"github.com/hymkor/gmnlisp"
)

var flagExecute = flag.String("e", "", "execute string")

func mains(args []string) error {
	var last gmnlisp.Node
	var err error

	lisp := gmnlisp.New()

	if *flagExecute != "" {
		last, err = lisp.Interpret(*flagExecute)
		if err != nil {
			return err
		}
	}
	for _, fname := range args {
		var script []byte

		script, err = os.ReadFile(fname)
		if err != nil {
			return err
		}
		last, err = lisp.InterpretBytes(script)
		if err != nil {
			return err
		}
	}
	lisp.Interpret("(terpri)")
	last.PrintTo(os.Stdout)
	lisp.Interpret("(terpri)")
	return nil
}

func main() {
	flag.Parse()
	if err := mains(flag.Args()); err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		os.Exit(1)
	}
}
