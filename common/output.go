package common

import (
	"context"
	"fmt"
	"io"

	. "github.com/hymkor/gmnlisp"
)

var emptyString UTF8String

func getWriterAndString(ctx context.Context, w *World, n Node) (io.Writer, StringTypes, error) {
	_s, n, err := w.ShiftAndEvalCar(ctx, n)
	if err != nil {
		return nil, emptyString, err
	}
	s, ok := _s.(StringTypes)
	if !ok {
		return nil, emptyString, fmt.Errorf("%w `%s`", ErrExpectedString, ToString(_s, PRINT))
	}
	var writer io.Writer
	if HasValue(n) {
		_writer, n, err := w.ShiftAndEvalCar(ctx, n)
		if err != nil {
			return nil, emptyString, err
		}
		writer, ok = _writer.(io.Writer)
		if !ok {
			return nil, emptyString, fmt.Errorf("Expected Writer `%s`", ToString(_writer, PRINT))
		}
		if HasValue(n) {
			return nil, emptyString, ErrTooManyArguments
		}
	} else {
		writer = w.Stdout()
	}
	return writer, s, nil
}

func funWrite(ctx context.Context, w *World, args []Node, kwargs map[Keyword]Node) (Node, error) {
	var writer io.Writer
	if writerNode, ok := kwargs[":stream"]; ok {
		if _writer, ok := writerNode.(io.Writer); ok {
			writer = _writer
		}
	} else {
		writer = w.Stdout()
	}
	args[0].PrintTo(writer, PRINT)
	return args[0], nil
}

func cmdWriteLine(ctx context.Context, w *World, n Node) (Node, error) {
	writer, s, err := getWriterAndString(ctx, w, n)
	if err != nil {
		return nil, err
	}
	fmt.Fprintln(writer, s.String())
	return Null, nil
}

func funTerpri(ctx context.Context, w *World, argv []Node) (Node, error) {
	var out io.Writer
	if len(argv) > 1 {
		return nil, ErrTooManyArguments
	}
	if len(argv) <= 0 {
		out = w.Stdout()
	} else {
		var ok bool
		out, ok = argv[0].(io.Writer)
		if !ok {
			return nil, ErrExpectedWriter
		}
	}
	fmt.Fprintln(out)
	return Null, nil
}

func cmdPrinX(ctx context.Context, w *World, argv []Node, f func(node Node, out io.Writer)) (Node, error) {
	f(argv[0], w.Stdout())
	return argv[0], nil
}

func funPrint(ctx context.Context, w *World, argv []Node) (Node, error) {
	return cmdPrinX(ctx, w, argv, func(node Node, out io.Writer) {
		out.Write([]byte{'\n'})
		node.PrintTo(out, PRINT)
		out.Write([]byte{' '})
	})
}

func funPrin1(ctx context.Context, w *World, argv []Node) (Node, error) {
	return cmdPrinX(ctx, w, argv, func(node Node, out io.Writer) {
		node.PrintTo(out, PRINT)
	})
}

func funPrinc(ctx context.Context, w *World, argv []Node) (Node, error) {
	return cmdPrinX(ctx, w, argv, func(node Node, out io.Writer) {
		node.PrintTo(out, PRINC)
	})
}
