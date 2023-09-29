package main

import (
	"bufio"
	"context"
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"runtime"
	"strings"

	"github.com/hymkor/gmnlisp"
	_ "github.com/hymkor/gmnlisp/pkg/command"
	_ "github.com/hymkor/gmnlisp/pkg/regexp"
	_ "github.com/hymkor/gmnlisp/pkg/wildcard"
	"github.com/mattn/go-colorable"
	"github.com/nyaosorg/go-readline-ny"
	"github.com/nyaosorg/go-readline-ny/simplehistory"
)

var version string = "snapshot"

var flagExecute = flag.String("e", "", "execute string")

func setArgv(w *gmnlisp.World, args []string) {
	posixArgv := []gmnlisp.Node{}
	for _, s := range args {
		posixArgv = append(posixArgv, gmnlisp.String(s))
	}
	w.SetOrDefineParameter(gmnlisp.NewSymbol("*posix-argv*"), gmnlisp.List(posixArgv...))
}

func defaultPrompt(w io.Writer) (int, error) {
	return io.WriteString(w, "gmnlisp> ")
}

type Coloring struct {
	bits int
	last rune
}

func (c *Coloring) Init() readline.ColorSequence {
	c.bits = 0
	c.last = 0
	return readline.White
}

func (c *Coloring) Next(ch rune) readline.ColorSequence {
	prebits := c.bits
	if c.last != '\\' && ch == '"' {
		c.bits ^= 1
	}
	var color readline.ColorSequence
	if (c.bits&1) != 0 || (prebits&1) != 0 {
		color = readline.Magenta
	} else if ch == '(' || ch == ')' {
		color = readline.Cyan
	} else if ch == '\\' || ch == '#' || ch == '\'' {
		color = readline.Red
	} else {
		color = readline.White
	}
	c.last = ch
	return color
}

func interactive(lisp *gmnlisp.World) error {
	history := simplehistory.New()
	editor := readline.Editor{
		PromptWriter:   defaultPrompt,
		Writer:         colorable.NewColorableStdout(),
		History:        history,
		Coloring:       &Coloring{},
		HistoryCycling: true,
	}
	fmt.Printf("gmnlisp %s-%s-%s by %s\n",
		version,
		runtime.GOOS,
		runtime.GOARCH,
		runtime.Version())

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
				editor.PromptWriter = defaultPrompt
				continue
			}
			fmt.Printf("ERR=%s\n", err.Error())
			return nil
		}
		history.Add(line)
		buffer.WriteString(line)

		nodes, err := gmnlisp.ReadAll(strings.NewReader(buffer.String()))
		if err == gmnlisp.ErrTooShortTokens {
			editor.PromptWriter = func(w io.Writer) (int, error) {
				return io.WriteString(w, "> ")
			}
			buffer.Write([]byte{'\n'})
			continue
		}
		buffer.Reset()
		editor.PromptWriter = defaultPrompt
		if err != nil {
			fmt.Fprintln(os.Stderr, err.Error())
			continue
		}
		result, err := lisp.InterpretNodes(ctx, nodes)
		if err != nil {
			if errors.Is(err, gmnlisp.ErrQuit) {
				return nil
			}
			fmt.Fprintln(os.Stderr, err.Error())
			continue
		}
		result.PrintTo(os.Stdout, gmnlisp.PRINT)
		fmt.Println()
	}
}

func mains(args []string) error {
	var err error

	ctx := context.Background()
	lisp := gmnlisp.New()

	if *flagExecute != "" {
		setArgv(lisp, args)
		_, err = lisp.Interpret(ctx, *flagExecute)
	} else if len(args) > 0 {
		setArgv(lisp, args[1:])

		fd, err := os.Open(args[0])
		if err != nil {
			return err
		}
		defer fd.Close()

		br := bufio.NewReader(fd)
		magicByte, err := br.Peek(1)
		if err != nil {
			return fmt.Errorf("%s: %w", args[0], err)
		}
		if magicByte[0] == '#' || magicByte[0] == '@' {
			br.ReadString('\n')
		}
		script, err := io.ReadAll(br)
		if err != nil {
			return fmt.Errorf("%s: %w", args[0], err)
		}
		_, err = lisp.InterpretBytes(ctx, script)
		return err
	} else {
		return interactive(lisp)
	}
	return err
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
