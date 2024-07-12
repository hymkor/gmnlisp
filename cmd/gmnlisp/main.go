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
	"github.com/hymkor/go-multiline-ny"
	"github.com/mattn/go-colorable"
	"github.com/nyaosorg/go-readline-ny"
	"github.com/nyaosorg/go-readline-ny/simplehistory"
	"github.com/nyaosorg/go-readline-skk"
)

var version string = "snapshot"

var flagExecute = flag.String("e", "", "execute string")

func setArgv(w *gmnlisp.World, args []string) {
	posixArgv := []gmnlisp.Node{}
	for _, s := range args {
		posixArgv = append(posixArgv, gmnlisp.String(s))
	}
	w.DefineGlobal(gmnlisp.NewSymbol("*posix-argv*"), gmnlisp.List(posixArgv...))
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

type miniBuffer struct {
	ed     *multiline.Editor
	rewind func()
}

func (q *miniBuffer) Enter(w io.Writer, prompt string) (int, error) {
	q.rewind = q.ed.GotoEndLine()
	return io.WriteString(w, prompt)
}

func (q *miniBuffer) Leave(w io.Writer) (int, error) {
	q.rewind()
	return 0, nil
}

func (q *miniBuffer) Recurse() skk.MiniBuffer {
	return skk.MiniBufferOnCurrentLine{}
}

func interactive(lisp *gmnlisp.World) error {
	history := simplehistory.New()

	var editor multiline.Editor

	editor.SetHistory(history)
	editor.SetWriter(colorable.NewColorableStdout())
	editor.SetColoring(&skk.Coloring{Base: &Coloring{}})
	editor.SetPrompt(func(w io.Writer, i int) (int, error) {
		if i == 0 {
			return io.WriteString(w, "gmnlisp> ")
		} else {
			return fmt.Fprintf(w, "%7d> ", i+1)
		}
	})
	editor.SubmitOnEnterWhen(func(lines []string, csrline int) bool {
		quote := false
		count := 0
		for _, line := range lines {
			for _, c := range line {
				if c == '"' {
					quote = !quote
				}
				if !quote {
					if c == '(' {
						count++
					} else if c == ')' {
						count--
					}
				}
			}
		}
		return count == 0
	})

	if env := os.Getenv("GOREADLINESKK"); env != "" {
		_, err := skk.Config{
			BindTo:         &editor.LineEditor,
			MiniBuffer:     &miniBuffer{ed: &editor},
			KeepModeOnExit: true,
		}.SetupWithString(env)
		if err != nil {
			fmt.Fprintln(os.Stderr, err.Error())
		}
	}

	fmt.Printf("gmnlisp %s-%s-%s by %s\n",
		version,
		runtime.GOOS,
		runtime.GOARCH,
		runtime.Version())

	ctx := context.Background()
	for {
		lines, err := editor.Read(ctx)
		if err != nil {
			return err
		}
		code := strings.Join(lines, "\n")

		history.Add(code)

		result, err := lisp.Interpret(ctx, code)
		if err != nil {
			if errors.Is(err, gmnlisp.ErrQuit) {
				return nil
			}
			fmt.Fprintln(os.Stderr, err.Error())
			continue
		}
		if gmnlisp.IsSome(result) {
			fmt.Fprintln(os.Stdout, result.String())
		}
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
