package main

import (
	"flag"
	"fmt"
	"os"

	"github.com/hymkor/gommon"
)

var flagExecute = flag.String("e", "", "execute string")

func mains(args []string) error {
	var last gommon.Node
	var err error

	if *flagExecute != "" {
		last, err = gommon.Interpret(*flagExecute)
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
		last, err = gommon.Interpret(string(script))
		if err != nil {
			return err
		}
	}
	if !gommon.IsNull(last) {
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
