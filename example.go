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
		fmt.Printf("   %s\n-> ", code)
		compiled, err := gommon.ReadString(code)
		if err != nil {
			return err
		}
		result, err := compiled.Eval()
		if err != nil {
			return err
		}
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
