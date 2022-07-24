package gmnlisp

import (
	"context"
	"fmt"
	"io"
)

func getWriterAndString(ctx context.Context, w *World, n Node) (io.Writer, String, error) {
	_s, n, err := w.shiftAndEvalCar(ctx, n)
	if err != nil {
		return nil, "", err
	}
	s, ok := _s.(String)
	if !ok {
		return nil, "", fmt.Errorf("%w `%s`", ErrExpectedString, toString(_s))
	}
	var writer io.Writer
	if HasValue(n) {
		_writer, n, err := w.shiftAndEvalCar(ctx, n)
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

func cmdWrite(ctx context.Context, w *World, n Node) (Node, error) {
	writer, s, err := getWriterAndString(ctx, w, n)
	if err != nil {
		return nil, err
	}
	io.WriteString(writer, string(s))
	return s, nil
}

func cmdWriteLine(ctx context.Context, w *World, n Node) (Node, error) {
	writer, s, err := getWriterAndString(ctx, w, n)
	if err != nil {
		return nil, err
	}
	fmt.Fprintln(writer, string(s))
	return s, nil
}

func cmdTerpri(ctx context.Context, w *World, _ Node) (Node, error) {
	out, err := w.Stdout()
	if err != nil {
		return nil, err
	}
	fmt.Fprintln(out)
	return Null, nil
}

func cmdPrinX(ctx context.Context, w *World, n Node, f func(node Node, out io.Writer)) (Node, error) {
	var argv [1]Node
	if err := w.evalListAll(ctx, n, argv[:]); err != nil {
		return nil, err
	}
	out, err := w.Stdout()
	if err != nil {
		return nil, err
	}
	f(argv[0], out)
	return argv[0], nil
}

func cmdPrint(ctx context.Context, w *World, this Node) (Node, error) {
	return cmdPrinX(ctx, w, this, func(node Node, out io.Writer) {
		out.Write([]byte{'\n'})
		node.PrintTo(out, PRINT)
		out.Write([]byte{' '})
	})
}

func cmdPrin1(ctx context.Context, w *World, this Node) (Node, error) {
	return cmdPrinX(ctx, w, this, func(node Node, out io.Writer) {
		node.PrintTo(out, PRINT)
	})
}

func cmdPrinc(ctx context.Context, w *World, this Node) (Node, error) {
	return cmdPrinX(ctx, w, this, func(node Node, out io.Writer) {
		node.PrintTo(out, PRINC)
	})
}
