package main

import (
	"bufio"
	"bytes"
	"flag"
	"fmt"
	"io"
	"os"
	"regexp"
	"strings"

	"github.com/hymkor/gmnlisp"
	"github.com/nyaosorg/go-windows-mbcs"
)

var rxPattern = regexp.MustCompile(`%[^%]+%`)
var rxLisp = regexp.MustCompile(`^\s*\(`)

func replaceFile(lisp *gmnlisp.World, fname string) error {
	fd, err := os.Open(fname)
	if err != nil {
		return err
	}
	defer fd.Close()
	return replaceReader(lisp, fd)
}

func countNoClosedParen(b []byte) int {
	count := 0
	quote := false
	for _, c := range b {
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
	return count
}

func replaceReader(lisp *gmnlisp.World, fd io.Reader) error {
	br := bufio.NewReader(fd)
	for {
		line, err := br.ReadBytes('\n')
		if err != nil && err != io.EOF {
			return err
		}
		if rxLisp.Match(line) {
			if countNoClosedParen(line) == 0 {
				_, err := lisp.InterpretBytes(line)
				if err != nil {
					return err
				}
				continue
			}
			var buffer bytes.Buffer
			buffer.Write(line)

			for {
				line, err = br.ReadBytes('\n')
				if err != nil {
					return err
				}
				buffer.Write(line)
				if b := buffer.Bytes(); countNoClosedParen(b) == 0 {
					if _, err := lisp.InterpretBytes(b); err != nil {
						return err
					}
					break
				}
			}
			continue
		}
		line = rxPattern.ReplaceAllFunc(line, func(s []byte) []byte {
			name := string(s[1 : len(s)-1])
			value, err := lisp.Interpret(name)
			if err != nil {
				return s
			}
			var buffer bytes.Buffer
			value.PrintTo(&buffer, gmnlisp.PRINC)
			return buffer.Bytes()
		})
		os.Stdout.Write(line)
		if err == io.EOF {
			return nil
		}
	}
}

var flagAnsi = flag.Bool("ansi", false, "macro value is not UTF8 (ANSI)")

func mains(args []string) error {
	lisp := gmnlisp.New()

	fileCount := 0
	for _, arg := range args {
		pos := strings.IndexByte(arg, '=')
		if pos >= 0 {
			left := strings.ToUpper(arg[0:pos])
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
		if err := replaceReader(lisp, os.Stdin); err != nil {
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
