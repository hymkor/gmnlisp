package gmnlisp

import (
	"context"
	"errors"
	"fmt"
	"io"
	"strconv"
	"strings"
)

type runeWriter interface {
	io.Writer
	WriteRune(r rune) (int, error)
}

func printInt(w io.Writer, value Node, base int, args ...int) error {
	if base < 2 || base > 36 {
		return errors.New("domain error")
	}
	width := -1
	padding := -1
	if argc := len(args); argc >= 3 {
		return MakeError(ErrTooManyArguments, "printInt")
	} else if argc == 2 {
		width = args[0]
		padding = args[1]
	} else if argc == 1 {
		width = args[0]
		padding = ' '
	}

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

func funFormatObject(ctx context.Context, w *World, list []Node) (Node, error) {
	return tAndNilToWriter(ctx, w, list, func(writer Formatter, list []Node) error {
		var err error
		if IsNone(list[1]) { // ~a (AS-IS)
			_, err = tryPrintTo(writer, list[0], PRINC)
		} else { // ~s (S expression)
			_, err = tryPrintTo(writer, list[0], PRINT)
		}
		return err
	})
}

func funFormatChar(ctx context.Context, w *World, list []Node) (Node, error) {
	return tAndNilToWriter(ctx, w, list, func(writer Formatter, list []Node) error {
		r, err := ExpectClass[Rune](ctx, w, list[0])
		if err != nil {
			return err
		}
		_, err = writer.WriteRune(rune(r))
		return err
	})
}

func funFormatInteger(ctx context.Context, w *World, _args []Node) (Node, error) {
	return tAndNilToWriter(ctx, w, _args, func(writer Formatter, args []Node) error {
		radix, err := ExpectClass[Integer](ctx, w, args[1])
		if err != nil {
			return err
		}
		return printInt(writer, args[0], int(radix))
	})
}

func funFormatTab(ctx context.Context, w *World, writer, column Node) (Node, error) {
	return tAndNilToWriter(ctx, w, []Node{writer, column}, func(writer Formatter, args []Node) error {
		n, err := ExpectClass[Integer](ctx, w, args[0])
		if err != nil {
			return err
		}
		if n < 0 || n > 1024 {
			_, err := raiseProgramError(ctx, w, ErrIndexOutOfRange)
			return err
		}
		i := writer.Column()
		if i >= int(n) {
			writer.WriteByte(' ')
			return nil
		}
		for ; i < int(n); i++ {
			writer.WriteByte(' ')
		}
		return nil
	})
}

func printFloat(w runeWriter, value Node, mark byte, args ...int) error {
	width := -1
	prec := -1
	if argc := len(args); argc >= 3 {
		return MakeError(ErrTooManyArguments, "printFloat")
	} else if argc == 2 {
		width = args[0]
		prec = args[1]
	} else if argc == 1 {
		width = args[0]
	}
	var body string
	if d, ok := value.(Integer); ok {
		body = strconv.FormatFloat(float64(d), mark, prec, 64)
	} else if f, ok := value.(Float); ok {
		body = strconv.FormatFloat(float64(f), mark, prec, 64)
	} else {
		return ErrNotSupportType
	}
	if len(body) < width {
		for i := len(body); i < width; i++ {
			w.WriteRune(' ')
		}
	}
	io.WriteString(w, body)
	return nil
}

func funFormatFloat(ctx context.Context, w *World, args []Node) (Node, error) {
	return tAndNilToWriter(ctx, w, args, func(_writer Formatter, args []Node) error {
		return printFloat(_writer, args[0], 'f')
	})
}

func printSpaces(n int, w io.Writer) {
	for n > 0 {
		w.Write([]byte{' '})
		n--
	}
}

var NewLineOnFormat = []byte{'\n'}

type Formatter interface {
	Column() int
	Write([]byte) (int, error)
	WriteByte(byte) error
	WriteRune(rune) (int, error)
	WriteString(string) (int, error)
}

func formatSub(ctx context.Context, world *World, w Formatter, argv []Node) error {
	format, err := ExpectClass[String](ctx, world, argv[0])
	if err != nil {
		return err
	}
	argv = argv[1:]

	ok := true
	for ok && IsSome(format) {
		var c Rune

		c, format, ok = format.firstRuneAndRestString()
		if !ok {
			break
		}
		if c != '~' {
			tryPrintTo(w, c, PRINC)
			continue
		}
		if IsNone(format) {
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
		}
		parameter := []int{}
		for {
			if decimal := strings.IndexByte("0123456789", byte(c)); decimal >= 0 {
				for {
					if IsNone(format) {
						return ErrInvalidFormat
					}
					c, format, ok = format.firstRuneAndRestString()
					if !ok {
						return ErrInvalidFormat
					}
					d := strings.IndexByte("0123456789", byte(c))
					if d < 0 {
						parameter = append(parameter, decimal)
						break
					}
					decimal = decimal*10 + d
				}
			} else if c == '\'' {
				if IsNone(format) {
					return ErrInvalidFormat
				}
				c, format, ok = format.firstRuneAndRestString()
				if !ok {
					return ErrInvalidFormat
				}
				parameter = append(parameter, int(c))
				if IsNone(format) {
					return ErrInvalidFormat
				}
				c, format, ok = format.firstRuneAndRestString()
			} else if c == 'v' || c == 'V' {
				if len(argv) < 1 {
					return ErrTooFewArguments
				}
				decimal, err := ExpectClass[Integer](ctx, world, argv[0])
				if err != nil {
					return err
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
			if IsNone(format) {
				return ErrInvalidFormat
			}
			c, format, ok = format.firstRuneAndRestString()
			if !ok {
				break
			}
		}
		if c == 'T' {
			n := 8
			if len(parameter) > 0 {
				n = parameter[0]
			}
			for i := w.Column(); i < n; i++ {
				w.WriteByte(' ')
			}
			continue
		}
		if c == '&' {
			n := 1
			if len(parameter) >= 1 {
				n = parameter[0]
			}
			if w.Column() == 0 {
				n--
			}
			if n > 0 {
				for ; n >= 1; n-- {
					w.Write(NewLineOnFormat)
				}
			}
			continue
		}
		if c == '%' {
			if len(parameter) >= 1 {
				for n := parameter[0]; n >= 1; n-- {
					w.Write(NewLineOnFormat)
				}
			} else {
				w.Write(NewLineOnFormat)
			}
			continue
		}

		if len(argv) <= 0 {
			return ErrTooFewArguments
		}

		value := argv[0]
		argv = argv[1:]

		var err error
		switch c {
		case 'd', 'D':
			err = printInt(w, value, 10, parameter...)
		case 'x', 'X':
			err = printInt(w, value, 16, parameter...)
		case 'o', 'O':
			err = printInt(w, value, 8, parameter...)
		case 'b', 'B':
			err = printInt(w, value, 2, parameter...)
		case 'f', 'F':
			err = printFloat(w, value, 'f', parameter...)
		case 'e', 'E':
			err = printFloat(w, value, 'e', parameter...)
		case 'g', 'G':
			err = printFloat(w, value, 'g', parameter...)
		case 'a', 'A':
			n, err := tryPrintTo(w, value, PRINC)
			if err != nil {
				return err
			}
			if len(parameter) >= 1 {
				printSpaces(parameter[0]-n, w)
			}
		case 's', 'S':
			n, err := tryPrintTo(w, value, PRINT)
			if err != nil {
				return err
			}
			if len(parameter) >= 1 {
				printSpaces(parameter[0]-n, w)
			}
		default:
			err = fmt.Errorf("not support code '%c'", c)
		}
		if err != nil {
			return err
		}
	}
	return nil
}

func tAndNilToWriter(ctx context.Context, w *World, argv []Node, f func(Formatter, []Node) error) (Node, error) {
	if IsNone(argv[0]) {
		var buffer StringBuilder
		err := f(&buffer, argv[1:])
		return buffer.Sequence(), err
	}
	if True.Equals(argv[0], STRICT) {
		if W, ok := w.stdout._Writer.(Formatter); ok {
			return Null, f(W, argv[1:])
		}
		return Null, f(&_WriterNode{_Writer: w.stdout._Writer}, argv[1:])
	}
	type writerType interface {
		Node
		Formatter
	}
	writer, err := ExpectInterface[writerType](ctx, w, argv[0], outputFileStreamClass)
	if err != nil {
		return nil, err
	}
	return Null, f(writer, argv[1:])
}

func funFormat(ctx context.Context, world *World, argv []Node) (Node, error) {
	return tAndNilToWriter(ctx, world, argv, func(_w Formatter, _argv []Node) error {
		return formatSub(ctx, world, _w, _argv)
	})
}
