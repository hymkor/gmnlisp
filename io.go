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

func (d _Dummy) Equals(Node, EqlMode) bool {
	return false
}

func (d _Dummy) PrintTo(w io.Writer, m PrintMode) {
	io.WriteString(w, "(binary)")
}

func openAsRead(fname string) (Node, error) {
	type Reader struct {
		_Dummy
		*bufio.Reader
		io.Closer
	}
	file, err := os.Open(fname)
	if err != nil {
		return nil, err
	}
	return &Reader{
		Reader: bufio.NewReader(file),
		Closer: file,
	}, nil
}

func openAsWrite(fname string) (Node, error) {
	type Writer struct {
		_Dummy
		io.WriteCloser
	}
	file, err := os.Create(fname)
	if err != nil {
		return nil, err
	}
	return &Writer{
		WriteCloser: file,
	}, nil
}

func cmdOpen(w *World, n Node) (Node, error) {
	argv, err := listToSlice(n)
	if err != nil {
		return nil, err
	}
	if len(argv) <= 0 {
		return nil, ErrTooFewArguments
	}
	if len(argv) >= 3 {
		return nil, ErrTooManyArguments
	}
	_fname, ok := argv[0].(String)
	if !ok {
		return nil, fmt.Errorf("%w `%s`", ErrExpectedString, toString(_fname))
	}
	fname := string(_fname)
	if len(argv) < 2 {
		return openAsRead(fname)
	}
	_mode, ok := argv[1].(String)
	if !ok {
		return nil, fmt.Errorf("%w `%s`", ErrExpectedString, toString(_mode))
	}
	mode := string(_mode)

	if mode == "r" {
		return openAsRead(fname)
	} else if mode == "w" {
		return openAsWrite(fname)
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
	var writer io.Writer
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
	} else {
		writer, err = w.Stdout()
		if err != nil {
			return nil, "", err
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
