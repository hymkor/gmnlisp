package main

import (
	"flag"
	"fmt"
	"os"

	"github.com/hymkor/gmnlisp"
)

var flagExecute = flag.String("e", "", "execute string")

func setArgv(w *gmnlisp.World, args []string) {
	posixArgv := []gmnlisp.Node{}
	for _, s := range args {
		posixArgv = append(posixArgv, gmnlisp.String(s))
	}
	w.Set("*posix-argv*", gmnlisp.List(posixArgv...))
}

func mains(args []string) error {
	var last = gmnlisp.Null
	var err error

	lisp := gmnlisp.New()

	if *flagExecute != "" {
		setArgv(lisp, args)
		last, err = lisp.Interpret(*flagExecute)
	} else if len(args) > 0 {
		setArgv(lisp, args[1:])

		var script []byte
		script, err = os.ReadFile(args[0])
		if err != nil {
			return err
		}
		last, err = lisp.InterpretBytes(script)
	}
	if err != nil {
		return err
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
