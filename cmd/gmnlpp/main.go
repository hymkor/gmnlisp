package main

import (
	"bufio"
	"bytes"
	"context"
	"flag"
	"fmt"
	"io"
	"os"
	"regexp"
	"strings"

	"github.com/hymkor/gmnlisp"
	"github.com/nyaosorg/go-windows-mbcs"
)

func replaceFile(lisp *gmnlisp.World, fname string) error {
	fd, err := os.Open(fname)
	if err != nil {
		return err
	}
	defer fd.Close()
	return replaceReader(lisp, fd, os.Stdout, os.Stderr)
}

var (
	rxBeginning = regexp.MustCompile(`\A(?s).*?<%=?`)
	rxMiddle    = regexp.MustCompile(`(?s)%>.*?<%=?`)
	rxEnding    = regexp.MustCompile(`(?s)%>.*?\z`)
)

var unescapeSequenceReplacer = strings.NewReplacer(
	"\n", "\\n",
	"\r", "\\r",
	"\t", "\\t",
	"\b", "\\b",
	"\"", "\\\"",
)

var equalMark = false

func replaceFunc(source []byte) []byte {
	var buffer bytes.Buffer

	if equalMark {
		buffer.WriteString(")")
		equalMark = false
	}

	if bytes.HasPrefix(source, []byte{'%', '>'}) {
		source = source[2:]
	}
	if len(source) > 0 && source[0] == '\r' {
		source = source[1:]
	}
	if len(source) > 0 && source[0] == '\n' {
		source = source[1:]
	}
	if len(source) > 0 && source[len(source)-1] == '=' {
		source = source[:len(source)-1]
		equalMark = true
	}
	if bytes.HasSuffix(source, []byte{'<', '%'}) {
		source = source[:len(source)-2]
	}
	for {
		before, after, found := bytes.Cut(source, []byte{'\n'})
		if !found {
			if len(source) > 0 {
				fmt.Fprint(&buffer, `(write "`)
				unescapeSequenceReplacer.WriteString(&buffer, string(source))
				fmt.Fprintln(&buffer, `")`)
			}
			if equalMark {
				buffer.WriteString("(princ ")
			}
			return buffer.Bytes()
		}
		if len(before) > 0 && before[len(before)-1] == '\r' {
			before = before[:len(before)-1]
		}
		fmt.Fprint(&buffer, `(write-line "`)
		unescapeSequenceReplacer.WriteString(&buffer, string(before))
		fmt.Fprintln(&buffer, `")`)
		source = []byte(after)
	}
}

func replaceReader(lisp *gmnlisp.World, r io.Reader, w, sourceOut io.Writer) (err error) {
	source, err := io.ReadAll(r)
	if err != nil {
		return err
	}
	bw := bufio.NewWriter(w)

	source = rxBeginning.ReplaceAllFunc(source, replaceFunc)
	source = rxMiddle.ReplaceAllFunc(source, replaceFunc)
	source = rxEnding.ReplaceAllFunc(source, replaceFunc)

	sourceOut.Write(source)

	orgStdout := lisp.Stdout()
	lisp.SetStdout(bw)

	defer func() {
		if err == io.EOF {
			err = nil
		}
		bw.Flush()
		lisp.SetStdout(orgStdout)
	}()

	_, err = lisp.InterpretBytes(context.TODO(), source)
	return err
}

var flagAnsi = flag.Bool("ansi", false, "macro value is not UTF8 (ANSI)")

func mains(args []string) error {
	lisp := gmnlisp.New()

	fileCount := 0
	for _, arg := range args {
		pos := strings.IndexByte(arg, '=')
		if pos >= 0 {
			left := gmnlisp.Symbol(strings.ToUpper(arg[0:pos]))
			right := arg[pos+1:]
			if *flagAnsi {
				value, err := mbcs.UtoA(right, mbcs.ACP)
				if err != nil {
					return fmt.Errorf("%s: %w", arg, err)
				}
				lisp.Set(left, gmnlisp.String(string(value)))
			} else {
				lisp.Set(left, gmnlisp.String(right))
			}
		} else {
			if err := replaceFile(lisp, arg); err != nil {
				return err
			}
			fileCount++
		}
	}
	if fileCount <= 0 {
		if err := replaceReader(lisp, os.Stdin, os.Stdout, os.Stderr); err != nil {
			return err
		}
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
