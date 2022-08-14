package gmnlisp

import (
	"bufio"
	"context"
	"fmt"
	"io"
	"os"
	"strings"
)

func getWriterAndString(ctx context.Context, w *World, n Node) (io.Writer, String, error) {
	_s, n, err := w.shiftAndEvalCar(ctx, n)
	if err != nil {
		return nil, emptyString, err
	}
	s, ok := _s.(String)
	if !ok {
		return nil, emptyString, fmt.Errorf("%w `%s`", ErrExpectedString, toString(_s, PRINT))
	}
	var writer io.Writer
	if HasValue(n) {
		_writer, n, err := w.shiftAndEvalCar(ctx, n)
		if err != nil {
			return nil, emptyString, err
		}
		writer, ok = _writer.(io.Writer)
		if !ok {
			return nil, emptyString, fmt.Errorf("Expected Writer `%s`", toString(_writer, PRINT))
		}
		if HasValue(n) {
			return nil, emptyString, ErrTooManyArguments
		}
	} else {
		writer, err = w.Stdout()
		if err != nil {
			return nil, emptyString, err
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

func cmdPrinX(ctx context.Context, w *World, argv []Node, f func(node Node, out io.Writer)) (Node, error) {
	out, err := w.Stdout()
	if err != nil {
		return nil, err
	}
	f(argv[0], out)
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

type runeWriter interface {
	WriteRune(rune) (int, error)
	io.Writer
}

func formatInt(w io.Writer, format string, value Node) error {
	if d, ok := value.(Integer); ok {
		fmt.Fprintf(w, format, int(d))
		return nil
	} else if f, ok := value.(Float); ok {
		fmt.Fprintf(w, format, int(f))
		return nil
	}
	return ErrNotSupportType
}

func formatFloat(w io.Writer, format string, value Node) error {
	if d, ok := value.(Integer); ok {
		fmt.Fprintf(w, format, float64(d))
		return nil
	} else if f, ok := value.(Float); ok {
		fmt.Fprintf(w, format, float64(f))
		return nil
	}
	return ErrNotSupportType
}

func formatSub(w runeWriter, format String, argv []Node) (Node, error) {
	for len(format) > 0 {
		c := format[0]
		format = format[1:]
		if c != '~' {
			w.WriteRune(rune(c))
			continue
		}
		if len(format) == 0 {
			w.WriteRune('~')
			break
		}
		c = format[0]
		format = format[1:]
		if c == '~' {
			w.Write([]byte{'~'})
			continue
		} else if c == '%' {
			w.Write([]byte{'\n'})
			continue
		}

		var gofmt strings.Builder
		gofmt.WriteByte('%')
		width := strings.IndexByte("0123456789", byte(c))
		if width >= 0 {
			gofmt.WriteByte(byte(c))
			for {
				if len(format) <= 0 {
					return Null, nil
				}
				c = format[0]
				format = format[1:]
				w := strings.IndexByte("0123456789", byte(c))
				if w < 0 {
					break
				}
				width = width*10 + w
				gofmt.WriteByte(byte(c))
			}
		}

		value := argv[0]
		argv = argv[1:]

		var err error
		switch c {
		case 'd':
			gofmt.WriteByte('d')
			err = formatInt(w, gofmt.String(), value)
		case 'x':
			gofmt.WriteByte('X')
			err = formatInt(w, gofmt.String(), value)
		case 'o':
			gofmt.WriteByte('o')
			err = formatInt(w, gofmt.String(), value)
		case 'b':
			gofmt.WriteByte('b')
			err = formatInt(w, gofmt.String(), value)
		case 'f':
			gofmt.WriteByte('f')
			err = formatFloat(w, gofmt.String(), value)
		case 'e':
			gofmt.WriteByte('e')
			err = formatFloat(w, gofmt.String(), value)
		case 'g':
			gofmt.WriteByte('g')
			err = formatFloat(w, gofmt.String(), value)
		case 'a':
			n, _ := value.PrintTo(w, PRINC)
			for n < width {
				w.WriteRune(' ')
				n++
			}
		case 's':
			n, _ := value.PrintTo(w, PRINT)
			for n < width {
				w.WriteRune(' ')
				n++
			}
		default:
			err = fmt.Errorf("Not support code '%c'", c)
		}
		if err != nil {
			return nil, err
		}
	}
	return Null, nil
}

func funFormat(ctx context.Context, w *World, argv []Node) (Node, error) {
	if len(argv) < 2 {
		return nil, ErrTooFewArguments
	}
	format, ok := argv[1].(String)
	if !ok {
		return nil, ErrExpectedString
	}
	if output, ok := argv[0].(io.Writer); ok {
		w := bufio.NewWriter(output)
		_, err := formatSub(w, format, argv[2:])
		w.Flush()
		return Null, err
	}
	if IsNull(argv[0]) {
		var buffer strings.Builder
		_, err := formatSub(&buffer, format, argv[2:])
		return String(buffer.String()), err
	}
	if True.Equals(argv[0], STRICT) {
		w := bufio.NewWriter(os.Stdout)
		_, err := formatSub(w, format, argv[2:])
		w.Flush()
		return Null, err
	}
	return nil, ErrNotSupportType
}
