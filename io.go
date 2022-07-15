package gmnlisp

import (
	"bufio"
	"fmt"
	"io"
	"os"
)

type Dummy struct{}

func (d Dummy) Eval(*World) (Node, error) {
	return d, nil
}

func (d Dummy) Equals(Node) bool {
	return false
}

func (d Dummy) Equalp(Node) bool {
	return false
}

func (d Dummy) PrintTo(w io.Writer) {
	io.WriteString(w, "(binary)")
}

func (d Dummy) PrincTo(w io.Writer) {
	io.WriteString(w, "(binary)")
}

type Reader struct {
	Dummy
	scanner *bufio.Scanner
	io.Closer
}

func cmdOpen(w *World, n Node) (Node, error) {
	var argv [2]Node
	if err := w.evalListAll(n, argv[:]); err != nil {
		return nil, err
	}

	_fname, ok := argv[0].(String)
	if !ok {
		return nil, fmt.Errorf("%w `%s`", ErrExpectedString, toString(_fname))
	}
	fname := string(_fname)

	_mode, ok := argv[1].(String)
	if !ok {
		return nil, fmt.Errorf("%w `%s`", ErrExpectedString, toString(_mode))
	}
	mode := string(_mode)

	if mode == "r" {
		file, err := os.Open(fname)
		if err != nil {
			return nil, err
		}
		return &Reader{
			scanner: bufio.NewScanner(file),
			Closer:  file,
		}, nil
	}
	return nil, fmt.Errorf("no such a option `%s`", argv[1])
}

func cmdReadLine(w *World, n Node) (Node, error) {
	var argv [1]Node
	if err := w.evalListAll(n, argv[:]); err != nil {
		return nil, err
	}
	reader, ok := argv[0].(*Reader)
	if !ok {
		return nil, fmt.Errorf("Expected Reader `%s`", toString(argv[0]))
	}
	if !reader.scanner.Scan() {
		return Null, nil
	}
	return String(reader.scanner.Text()), nil
}

func cmdClose(w *World, n Node) (Node, error) {
	var argv [1]Node
	if err := w.evalListAll(n, argv[:]); err != nil {
		return nil, err
	}
	c, ok := argv[0].(io.Closer)
	if !ok {
		return nil, fmt.Errorf("Expected Reader `%s`", toString(argv[0]))
	}
	return Null, c.Close()
}