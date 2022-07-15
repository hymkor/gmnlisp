package gmnlisp

import (
	"bufio"
	"fmt"
	"io"
	"os"
)

type _Dummy struct{}

func (d _Dummy) Eval(*World) (Node, error) {
	return d, nil
}

func (d _Dummy) Equals(Node) bool {
	return false
}

func (d _Dummy) Equalp(Node) bool {
	return false
}

func (d _Dummy) PrintTo(w io.Writer) {
	io.WriteString(w, "(binary)")
}

func (d _Dummy) PrincTo(w io.Writer) {
	io.WriteString(w, "(binary)")
}

func cmdOpen(w *World, n Node) (Node, error) {
	type Reader struct {
		_Dummy
		*bufio.Reader
		io.Closer
	}
	type Writer struct {
		_Dummy
		io.WriteCloser
	}
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
			Reader: bufio.NewReader(file),
			Closer: file,
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

func chomp(s string) string {
	L := len(s)
	if L > 0 && s[L-1] == '\n' {
		L--
		s = s[:L]
		if L > 0 && s[L-1] == '\r' {
			s = s[:L-1]
		}
	}
	return s
}

func cmdReadLine(w *World, n Node) (Node, error) {
	type ReadStringer interface {
		ReadString(byte) (string, error)
	}

	var argv [1]Node
	if err := w.evalListAll(n, argv[:]); err != nil {
		return nil, err
	}
	r, ok := argv[0].(ReadStringer)
	if !ok {
		return nil, fmt.Errorf("Expected Reader `%s`", toString(argv[0]))
	}
	s, err := r.ReadString('\n')
	if err == io.EOF {
		return Null, nil
	}
	return String(chomp(s)), err
}

func getWriterAndString(w *World, n Node) (io.Writer, String, error) {
	_s, n, err := w.shiftAndEvalCar(n)
	if err != nil {
		return nil, "", err
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
		return nil, fmt.Errorf("Expected Closer `%s`", toString(argv[0]))
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
