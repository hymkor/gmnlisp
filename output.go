package gmnlisp

import (
	"bufio"
	"context"
	"fmt"
	"io"
	"os"
	"strconv"
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

func printInt(w io.Writer, base, width, padding int, value Node) error {
	var body string
	if d, ok := value.(Integer); ok {
		body = strconv.FormatInt(int64(d), base)
	} else if f, ok := value.(Float); ok {
		body = strconv.FormatInt(int64(f), base)
	} else {
		return ErrNotSupportType
	}
	if len(body) < width {
		if padding <= 0 {
			padding = ' '
		}
		for i := len(body); i < width; i++ {
			w.Write([]byte{byte(padding)})
		}
	}
	io.WriteString(w, strings.ToUpper(body))
	return nil
}

func printFloat(w io.Writer, mark, width, prec int, value Node) error {
	if prec <= 0 {
		prec = -1
	}
	var body string
	if d, ok := value.(Integer); ok {
		body = strconv.FormatFloat(float64(d), byte(mark), prec, 64)
	} else if f, ok := value.(Float); ok {
		body = strconv.FormatFloat(float64(f), byte(mark), prec, 64)
	} else {
		return ErrNotSupportType
	}
	if len(body) < width {
		for i := len(body); i < width; i++ {
			w.Write([]byte{' '})
		}
	}
	io.WriteString(w, body)
	return nil
}

func printSpaces(n int, w io.Writer) {
	for n > 0 {
		w.Write([]byte{' '})
		n--
	}
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
		parameter := []int{}
		for {
			if decimal := strings.IndexByte("0123456789", byte(c)); decimal >= 0 {
				for {
					if len(format) <= 0 {
						return Null, ErrInvalidFormat
					}
					c = format[0]
					format = format[1:]
					d := strings.IndexByte("0123456789", byte(c))
					if d < 0 {
						parameter = append(parameter, decimal)
						break
					}
					decimal = decimal*10 + d
				}
			} else if c == '\'' {
				if len(format) < 2 {
					return Null, ErrInvalidFormat
				}
				parameter = append(parameter, int(format[0]))
				c = format[1]
				format = format[2:]
			} else if c == 'v' || c == 'V' {
				if len(argv) < 1 {
					return nil, ErrTooFewArguments
				}
				decimal, ok := argv[0].(Integer)
				if !ok {
					return nil, ErrExpectedNumber
				}
				parameter = append(parameter, int(decimal))
			} else if c == '#' {
				parameter = append(parameter, int(len(argv)))
			} else {
				break
			}
			if c != ',' {
				break
			}
			if len(format) <= 0 {
				return nil, ErrInvalidFormat
			}
			c = format[0]
			format = format[1:]
		}

		padding := -1
		width := -1
		if len(parameter) >= 1 {
			width = parameter[0]
			if len(parameter) >= 2 && parameter[1] == '0' {
				padding = '0'
			}
		}

		value := argv[0]
		argv = argv[1:]

		var err error
		switch c {
		case 'd':
			err = printInt(w, 10, width, padding, value)
		case 'x':
			err = printInt(w, 16, width, padding, value)
		case 'o':
			err = printInt(w, 8, width, padding, value)
		case 'b':
			err = printInt(w, 2, width, padding, value)
		case 'f':
			err = printFloat(w, 'f', width, padding, value)
		case 'e':
			err = printFloat(w, 'e', width, padding, value)
		case 'g':
			err = printFloat(w, 'g', width, padding, value)
		case 'a':
			n, _ := value.PrintTo(w, PRINC)
			printSpaces(width-n, w)
		case 's':
			n, _ := value.PrintTo(w, PRINT)
			printSpaces(width-n, w)
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
