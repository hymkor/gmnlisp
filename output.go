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

func getWriterAndString(ctx context.Context, w *World, n Node) (io.Writer, StringTypes, error) {
	_s, n, err := w.ShiftAndEvalCar(ctx, n)
	if err != nil {
		return nil, emptyString, err
	}
	s, ok := _s.(StringTypes)
	if !ok {
		return nil, emptyString, fmt.Errorf("%w `%s`", ErrExpectedString, toString(_s, PRINT))
	}
	var writer io.Writer
	if HasValue(n) {
		_writer, n, err := w.ShiftAndEvalCar(ctx, n)
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

func funFormatChar(_ context.Context, _ *World, list []Node) (Node, error) {
	writer, ok := list[0].(runeWriter)
	if !ok {
		return nil, ErrExpectedWriter
	}
	r, ok := list[1].(Rune)
	if !ok {
		return nil, ErrExpectedCharacter
	}
	writer.WriteRune(rune(r))
	return Null, nil
}

func funFormatInteger(_ context.Context, _ *World, list []Node) (Node, error) {
	writer, ok := list[0].(io.Writer)
	if !ok {
		return nil, ErrExpectedWriter
	}
	radix, ok := list[2].(Integer)
	if !ok {
		return nil, ErrExpectedNumber
	}
	return Null, printInt(writer, int(radix), 0, 0, list[1])
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

func formatSub(w runeWriter, format StringTypes, argv []Node) (Node, error) {
	var ok bool = true
	for ok && HasValue(format) {
		var c Rune

		c, format, ok = format.firstRuneAndRestString()
		if !ok {
			break
		}
		if c != '~' {
			c.PrintTo(w, PRINC)
			continue
		}
		if IsNull(format) {
			w.WriteRune('~')
			break
		}
		c, format, ok = format.firstRuneAndRestString()
		if !ok {
			break
		}
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
					if IsNull(format) {
						return Null, ErrInvalidFormat
					}
					c, format, ok = format.firstRuneAndRestString()
					if !ok {
						return Null, ErrInvalidFormat
					}
					d := strings.IndexByte("0123456789", byte(c))
					if d < 0 {
						parameter = append(parameter, decimal)
						break
					}
					decimal = decimal*10 + d
				}
			} else if c == '\'' {
				if IsNull(format) {
					return Null, ErrInvalidFormat
				}
				c, format, ok = format.firstRuneAndRestString()
				if !ok {
					return Null, ErrInvalidFormat
				}
				parameter = append(parameter, int(c))
				if IsNull(format) {
					return Null, ErrInvalidFormat
				}
				c, format, ok = format.firstRuneAndRestString()
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
			if IsNull(format) {
				return nil, ErrInvalidFormat
			}
			c, format, ok = format.firstRuneAndRestString()
			if !ok {
				break
			}
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

var defFormat = &Function{Min: 2, F: funFormat}

func funFormat(ctx context.Context, w *World, argv []Node) (Node, error) {
	format, ok := argv[1].(StringTypes)
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
