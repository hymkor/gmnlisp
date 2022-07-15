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

type Writer struct {
	Dummy
	io.WriteCloser
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
	} else if mode == "w" {
		file, err := os.Create(fname)
		if err != nil {
			return nil, err
		}
		return &Writer{
			WriteCloser: file,
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

func getWriterAndString(w *World, n Node) (io.Writer, String, error) {
	_s, n, err := w.shiftAndEvalCar(n)
	if err != nil {
		return Writer{}, "", err
	}
	s, ok := _s.(String)
	if !ok {
		return nil, "", fmt.Errorf("%w `%s`", ErrExpectedString, toString(_s))
	}
	var writer io.Writer = w.Stdout
	if HasValue(n) {
		_writer, n, err := w.shiftAndEvalCar(n)
		if err != nil {
			return nil, "", err
		}
		writer, ok = _writer.(io.Writer)
		if !ok {
			return nil, "", fmt.Errorf("Expected Writer `%s`", toString(_writer))
		}
		if HasValue(n) {
			return nil, "", ErrTooManyArguments
		}
	}
	return writer, s, nil
}

func cmdWrite(w *World, n Node) (Node, error) {
	writer, s, err := getWriterAndString(w, n)
	if err != nil {
		return nil, err
	}
	io.WriteString(writer, string(s))
	return s, nil
}

func cmdWriteLine(w *World, n Node) (Node, error) {
	writer, s, err := getWriterAndString(w, n)
	if err != nil {
		return nil, err
	}
	fmt.Fprintln(writer, string(s))
	return s, nil
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

func cmdWhile(w *World, n Node) (Node, error) {
	cons, ok := n.(*Cons)
	if !ok {
		return nil, ErrTooFewArguments
	}
	cond := cons.Car
	statements := cons.Cdr
	var last Node = Null
	for {
		cont, err := cond.Eval(w)
		if err != nil {
			return nil, err
		}
		if IsNull(cont) {
			return last, nil
		}
		last, err = progn(w, statements)
		if err != nil {
			return nil, err
		}
	}
}
