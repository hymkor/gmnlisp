package main

import (
	"bufio"
	"context"
	_ "embed"
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"regexp"
	"runtime"
	"strings"

	"github.com/hymkor/gmnlisp"
	_ "github.com/hymkor/gmnlisp/command"
	_ "github.com/hymkor/gmnlisp/eval"
	_ "github.com/hymkor/gmnlisp/regexp"
	_ "github.com/hymkor/gmnlisp/wildcard"
	"github.com/hymkor/go-multiline-ny"
	"github.com/hymkor/go-multiline-ny/completion"
	"github.com/mattn/go-colorable"
	"github.com/nyaosorg/go-readline-ny"
	"github.com/nyaosorg/go-readline-ny/keys"
	"github.com/nyaosorg/go-readline-ny/simplehistory"
	"github.com/nyaosorg/go-readline-skk"
)

var version string = "snapshot"

var (
	flagExecute = flag.String("e", "", "Execute string")
	flagPrint   = flag.Bool("p", false, "Print the result of the last evaluated expression")
	flagStrict  = flag.Bool("strict", false, "strict mode")
)

func setArgv(w *gmnlisp.World, args []string) {
	var argv gmnlisp.Node = gmnlisp.Null
	for i := len(args) - 1; i >= 0; i-- {
		argv = &gmnlisp.Cons{Car: gmnlisp.String(args[i]), Cdr: argv}
	}
	w.DefineGlobal(gmnlisp.NewSymbol("*argv*"), argv)
	w.DefineGlobal(gmnlisp.NewSymbol("*posix-argv*"), argv)
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

func hasPrefix(s, sub string) bool {
	return len(s) >= len(sub) && strings.EqualFold(s[:len(sub)], sub)
}

const (
	completeBoth        = 3
	completeFunction    = 2
	completeNonFunction = 1
)

func whatIsToComplete(fields []string) int {
	L := len(fields)
	if L >= 3 {
		if fields[L-3] == "#" && fields[L-2] == "'" {
			return completeFunction
		}
	}
	if L >= 2 {
		lastlast := fields[L-2]
		if lastlast == "'" {
			return completeBoth
		} else if strings.HasSuffix(lastlast, "(") {
			return completeFunction
		} else if strings.EqualFold(lastlast, "function") {
			return completeFunction
		}
	}
	return completeNonFunction
}

func interactive(lisp *gmnlisp.World) error {
	history := simplehistory.New()

	var editor multiline.Editor

	stdout := colorable.NewColorableStdout()
	editor.SetHistory(history)
	editor.SetWriter(stdout)
	var kc kakkoColor
	editor.Highlight = []readline.Highlight{
		kc.newKakkoLevel("\x1B[0;31;22m"), // red
		kc.newKakkoLevel("\x1B[0;33;22m"), // yellow
		kc.newKakkoLevel("\x1B[0;32;22m"), // green
		kc.newKakkoLevel("\x1B[0;36;22m"), // cyan
		kc.newKakkoLevel("\x1B[0;35;22m"), // magenta

		{Pattern: regexp.MustCompile(`[\\#']+`), Sequence: "\x1B[0;32;1m"}, // green

		{Pattern: regexp.MustCompile(`"([^"]*\\")*[^"]*$|"([^"]*\\")*[^"]*"`), Sequence: "\x1B[0;35;1m"}, // magenta
	}
	editor.ResetColor = "\x1B[0m"
	editor.DefaultColor = "\x1B[0;37;1m"
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
	ctx := context.Background()
	lisp.InterpretNodes(ctx, nil) // initialize
	editor.BindKey(keys.CtrlI, &completion.CmdCompletionOrList{
		// Characters listed here are excluded from completion.
		Delimiter: "()&|#'",
		// Enclose candidates with these characters when they contain spaces
		Enclosure: `"`,
		// String to append when only one candidate remains
		Postfix: " ",
		// Function for listing candidates
		Candidates: func(fields []string) ([]string, []string) {
			word := fields[len(fields)-1]
			var symbols = []string{}
			flag := whatIsToComplete(fields)
			if (flag & completeFunction) != 0 {
				lisp.FuncRange(func(key gmnlisp.Symbol, _ gmnlisp.Callable) bool {
					if k := key.String(); hasPrefix(k, word) {
						symbols = append(symbols, strings.ToLower(k))
					}
					return true
				})
			}
			if (flag & completeNonFunction) != 0 {
				lisp.Range(func(key gmnlisp.Symbol, _ gmnlisp.Node) bool {
					if k := key.String(); hasPrefix(k, word) {
						symbols = append(symbols, strings.ToLower(k))
					}
					return true
				})
			}
			return symbols, symbols
		},
	})
	fmt.Printf("gmnlisp %s-%s-%s by %s\n",
		version,
		runtime.GOOS,
		runtime.GOARCH,
		runtime.Version())

	for {
		lines, err := editor.Read(ctx)
		if err != nil {
			return err
		}
		if n := len(lines); n > 1 {
			fmt.Fprintf(stdout, "\r\x1B[%dA", n-1)
			for ; n > 1; n-- {
				fmt.Fprintln(stdout, "         ")
			}
		}
		code := strings.Join(lines, "\n")

		history.Add(code)

		lisp.SetStdout(os.Stdout) // reset the flag for `~&` in (format)
		lisp.SetErrout(os.Stderr) // reset the flag for `~&` in (format)
		result, err := lisp.Interpret(ctx, code)
		if err != nil {
			if errors.Is(err, gmnlisp.ErrQuit) {
				return nil
			}
			fmt.Fprintln(os.Stderr, err.Error())
			continue
		}
		if result == nil {
			result = gmnlisp.Null
		}
		if v, ok := result.(fmt.GoStringer); ok {
			fmt.Printf("%s\n", v.GoString())
		} else {
			fmt.Printf("%s\n", result.String())
		}
	}
}

//go:embed startup.lsp
var startupCode string

func mains(args []string) error {
	var err error

	ctx := context.Background()
	lisp := gmnlisp.New()
	if *flagStrict {
		lisp.StrictMode = true
	}
	executable := os.Args[0]
	if value, err := os.Executable(); err == nil {
		executable = value
	}
	lisp.DefineGlobal(gmnlisp.NewSymbol("*executable-name*"), gmnlisp.String(executable))
	lisp.DefineGlobal(gmnlisp.NewSymbol("*temp-dir*"), gmnlisp.String(os.TempDir()))
	lisp.DefineGlobal(gmnlisp.NewSymbol("*dev-null*"), gmnlisp.String(os.DevNull))
	if _, err := lisp.Interpret(ctx, startupCode); err != nil {
		return err
	}

	if *flagExecute != "" {
		setArgv(lisp, args)
		var v gmnlisp.Node
		v, err = lisp.Interpret(ctx, *flagExecute)
		if *flagPrint {
			fmt.Printf("%#v\n", v)
		}
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
		v, err := lisp.InterpretBytes(ctx, script)
		if *flagPrint {
			fmt.Printf("%#v\n", v)
		}
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
