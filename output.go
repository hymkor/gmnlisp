package gmnlisp

import (
	"fmt"
	"io"
)

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

func cmdTerpri(w *World, _ Node) (Node, error) {
	out, err := w.Stdout()
	if err != nil {
		return nil, err
	}
	fmt.Fprintln(out)
	return Null, nil
}

func cmdPrinX(w *World, n Node, f func(node Node, out io.Writer)) (Node, error) {
	var argv [1]Node
	if err := w.evalListAll(n, argv[:]); err != nil {
		return nil, err
	}
	out, err := w.Stdout()
	if err != nil {
		return nil, err
	}
	f(argv[0], out)
	return argv[0], nil
}

func cmdPrint(w *World, this Node) (Node, error) {
	if _, err := cmdTerpri(w, this); err != nil {
		return nil, err
	}
	return cmdPrinX(w, this, func(node Node, out io.Writer) {
		node.PrintTo(out, PRINT)
	})
}

func cmdPrin1(w *World, this Node) (Node, error) {
	return cmdPrinX(w, this, func(node Node, out io.Writer) {
		node.PrintTo(out, PRINT)
	})
}

func cmdPrinc(w *World, this Node) (Node, error) {
	return cmdPrinX(w, this, func(node Node, out io.Writer) {
		node.PrintTo(out, PRINC)
	})
}
