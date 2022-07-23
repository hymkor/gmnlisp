package main

import (
	"bufio"
	"flag"
	"fmt"
	"io"
	"os"
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
	return replaceReader(lisp, fd, os.Stdout)
}

func replaceReader(lisp *gmnlisp.World, r io.Reader, w io.Writer) (err error) {
	br := bufio.NewReader(r)
	bw := bufio.NewWriter(w)
	defer func() {
		if err == io.EOF {
			err = nil
		}
		bw.Flush()
	}()

	for {
		var ch rune
		ch, _, err = br.ReadRune()
		if err != nil {
			return
		}
		if ch == '(' {
			ch, _, err = br.ReadRune()
			if err != nil {
				return
			}
			if ch != '%' {
				bw.WriteRune('(')
				bw.WriteRune(ch)
				continue
			}
			ch, _, err = br.ReadRune()
			if err != nil {
				return
			}
			evalStyle := false
			if ch == '=' {
				evalStyle = true
			} else {
				br.UnreadRune()
			}

			var buffer strings.Builder
			for {
				ch, _, err = br.ReadRune()
				if err != nil {
					return
				}
				if ch == '%' {
					ch, _, err = br.ReadRune()
					if err != nil {
						return
					}
					if ch == ')' {
						var value gmnlisp.Node

						bw.Flush()
						value, err = lisp.Interpret(buffer.String())
						if err != nil {
							return
						}
						if evalStyle {
							value.PrintTo(bw, gmnlisp.PRINC)
						}
						ch, _, err = br.ReadRune()
						if err != nil {
							return
						}
						if ch == '\r' {
							ch, _, err = br.ReadRune()
							if err != nil {
								return
							}
						}
						if ch != '\n' {
							br.UnreadRune()
						}
						break
					}
					buffer.WriteRune('%')
				}
				buffer.WriteRune(ch)
			}
			continue
		}
		bw.WriteRune(ch)
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
		if err := replaceReader(lisp, os.Stdin, os.Stdout); err != nil {
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
