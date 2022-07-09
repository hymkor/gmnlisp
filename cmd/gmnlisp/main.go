package main

import (
	"flag"
	"fmt"
	"os"

	"github.com/hymkor/gommon"
)

var flagExecute = flag.String("e", "", "execute string")

func mains(args []string) error {
	if *flagExecute != "" {
		result, err := gommon.Interpret(*flagExecute)
		if err != nil {
			return err
		}
		result.PrintTo(os.Stdout)
		fmt.Println()
	}
	for _, fname := range args {
		script, err := os.ReadFile(fname)
		if err != nil {
			return err
		}
		result, err := gommon.Interpret(string(script))
		if err != nil {
			return err
		}
		result.PrintTo(os.Stdout)
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
