//go:build run
// +build run

package main

import (
	"fmt"
	"github.com/hymkor/gommon"
	"os"
)

func mains(args []string) error {
	for _, code := range args {
		fmt.Printf("   %s\n", code)
		result, err := gommon.Interpret(code)
		if err != nil {
			return err
		}
		fmt.Print("-> ")
		result.WriteTo(os.Stdout)
		fmt.Println()
	}
	return nil
}

func main() {
	if err := mains(os.Args[1:]); err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		os.Exit(1)
	}
}
