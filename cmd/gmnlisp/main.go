package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/hymkor/gmnlisp"
	"github.com/mattn/go-colorable"
	"github.com/nyaosorg/go-readline-ny"
	"github.com/nyaosorg/go-readline-ny/coloring"
	"github.com/nyaosorg/go-readline-ny/simplehistory"
)

var flagExecute = flag.String("e", "", "execute string")

func setArgv(w *gmnlisp.World, args []string) {
	posixArgv := []gmnlisp.Node{}
	for _, s := range args {
		posixArgv = append(posixArgv, gmnlisp.String(s))
	}
	w.Set("*posix-argv*", gmnlisp.List(posixArgv...))
}

func defaultPrompt() (int, error) {
	return fmt.Print("gmnlisp> ")
}

func interactive(lisp *gmnlisp.World) error {
	history := simplehistory.New()
	editor := readline.Editor{
		Prompt:         defaultPrompt,
		Writer:         colorable.NewColorableStdout(),
		History:        history,
		Coloring:       &coloring.VimBatch{},
		HistoryCycling: true,
	}
	ctx := context.Background()
	var buffer strings.Builder
	for {
		line, err := editor.ReadLine(ctx)
		if err != nil {
			if err == io.EOF { // Ctrl-D
				return nil
			}
			if err == readline.CtrlC {
				buffer.Reset()
				editor.Prompt = defaultPrompt
				continue
			}
			fmt.Printf("ERR=%s\n", err.Error())
			return nil
		}
		buffer.WriteString(line)

		nodes, err := gmnlisp.ReadString(buffer.String())
		if err == gmnlisp.ErrTooShortTokens {
			editor.Prompt = func() (int, error) {
				return fmt.Print("> ")
			}
			buffer.Write([]byte{'\n'})
			continue
		}
		buffer.Reset()
		editor.Prompt = defaultPrompt
		if err != nil {
			fmt.Fprintln(os.Stderr, err.Error())
			continue
		}
		result, err := nodes.Eval(lisp)
		if err != nil {
			if errors.Is(err, gmnlisp.ErrQuit) {
				return nil
			}
			fmt.Fprintln(os.Stderr, err.Error())
			continue
		}
		result.PrintTo(os.Stdout)
		fmt.Println()
	}
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
	} else {
		return interactive(lisp)
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
		if !errors.Is(err, gmnlisp.ErrQuit) {
			fmt.Fprintln(os.Stderr, err.Error())
			os.Exit(1)
		}
	}
}
