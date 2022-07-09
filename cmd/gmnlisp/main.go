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

	if *flagExecute != "" {
		last, err = gmnlisp.Interpret(*flagExecute)
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
		last, err = gmnlisp.Interpret(string(script))
		if err != nil {
			return err
		}
	}
	if !gmnlisp.IsNull(last) {
		fmt.Println()
		last.PrintTo(os.Stdout)
		fmt.Println()
	}
	return nil
}

func main() {
	flag.Parse()
	if err := mains(flag.Args()); err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		os.Exit(1)
	}
}
