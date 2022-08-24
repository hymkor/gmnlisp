package gmnlisp

import (
	"bufio"
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"strings"
)

type _Dummy struct{}

func (d _Dummy) Eval(context.Context, *World) (Node, error) {
	return d, nil
}

func (d _Dummy) Equals(Node, EqlMode) bool {
	return false
}

func (d _Dummy) PrintTo(w io.Writer, m PrintMode) (int, error) {
	return io.WriteString(w, "(binary)")
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

func cmdOpen(ctx context.Context, w *World, n Node) (Node, error) {
	fname, n, err := w.shiftAndEvalCar(ctx, n)
	if err != nil {
		return nil, err
	}
	_fname, ok := fname.(String)
	if !ok {
		return nil, fmt.Errorf("%w `%s`", ErrExpectedString, toString(_fname, PRINT))
	}
	if IsNull(n) {
		return openAsRead(string(_fname))
	}

	mode, n, err := w.shiftAndEvalCar(ctx, n)
	if HasValue(n) {
		return nil, ErrTooManyArguments
	}
	_mode, ok := mode.(String)
	if !ok {
		return nil, fmt.Errorf("%w `%s`", ErrExpectedString, toString(_mode, PRINT))
	}

	var result Node
	switch string(_mode) {
	case "r":
		result, err = openAsRead(string(_fname))
	case "w":
		result, err = openAsWrite(string(_fname))
	default:
		return nil, fmt.Errorf("no such a option `%s`", string(_mode))
	}
	if errors.Is(err, os.ErrNotExist) {
		return Null, nil
	}
	return result, err
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

var defReadLine = &Function{Max: 3, F: funReadLine}

func funReadLine(_ context.Context, w *World, argv []Node) (Node, error) {
	type ReadStringer interface {
		ReadString(byte) (string, error)
	}
	var reader ReadStringer
	var eofFlag bool = true
	var eofValue Node = Null

	switch len(argv) {
	default:
		return nil, ErrTooManyArguments
	case 3:
		eofValue = argv[2]
		fallthrough
	case 2:
		eofFlag = HasValue(argv[1])
		fallthrough
	case 1:
		var ok bool
		reader, ok = argv[0].(ReadStringer)
		if !ok {
			return nil, fmt.Errorf("Expected Reader `%s`", toString(argv[0], PRINT))
		}
	case 0:
		var err error
		reader, err = w.Stdin()
		if err != nil {
			return nil, err
		}
	}
	s, err := reader.ReadString('\n')
	if err == io.EOF {
		if eofFlag {
			return Null, io.EOF
		}
		return eofValue, nil
	}
	return String(chomp(s)), err
}

func funClose(_ context.Context, _ *World, argv []Node) (Node, error) {
	c, ok := argv[0].(io.Closer)
	if !ok {
		return nil, fmt.Errorf("Expected Closer `%s`", toString(argv[0], PRINT))
	}
	return Null, c.Close()
}

var defCommand = &Function{Min: 1, F: funCommand}

func funCommand(ctx context.Context, w *World, list []Node) (Node, error) {
	// from autolisp
	argv := make([]string, len(list))
	for i, value := range list {
		var buffer strings.Builder
		value.PrintTo(&buffer, PRINC)
		argv[i] = buffer.String()
	}

	var err error
	cmd := exec.CommandContext(ctx, argv[0], argv[1:]...)
	cmd.Stdout, err = w.Stdout()
	if err != nil {
		return nil, err
	}
	cmd.Stderr = os.Stderr
	cmd.Stdin = os.Stdin
	return Null, cmd.Run()
}

func cmdWithOpenFile(ctx context.Context, w *World, list Node) (Node, error) {
	var param Node
	var err error

	param, list, err = shift(list)
	if err != nil {
		return nil, err
	}

	var symbolNode Node
	symbolNode, param, err = shift(param)
	if err != nil {
		return nil, err
	}
	symbol, ok := symbolNode.(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}
	//var kwargs map[Keyword]Node
	var args []Node
	var kwargs map[Keyword]Node
	args, kwargs, err = listToKwargs(ctx, w, param)
	if err != nil {
		return nil, err
	}
	if len(args) < 1 {
		return nil, ErrTooFewArguments
	}
	if len(args) > 1 {
		return nil, ErrTooManyArguments
	}

	fname, ok := args[0].(String)
	if !ok {
		return nil, ErrExpectedString
	}
	var fdNode Node
	direction, ok := kwargs[":direction"]
	if !ok || direction == Keyword(":input") {
		fd, err := os.Open(string(fname))
		if err != nil {
			fdNode, ok = kwargs[":if-does-not-exist"]
			if !ok {
				return nil, err
			}
		} else {
			type Reader struct {
				_Dummy
				*bufio.Reader
			}
			fdNode = &Reader{Reader: bufio.NewReader(fd)}
			defer fd.Close()
		}
	} else if direction == Keyword(":output") {
		fd, err := os.Create(string(fname))
		if err != nil {
			return nil, err
		}
		type Writer struct {
			_Dummy
			io.Writer
		}
		fdNode = &Writer{Writer: fd}
		defer fd.Close()
	} else {
		return nil, fmt.Errorf("invalid :direction %s", toString(direction, PRINT))
	}
	newWorld := w.New(&_OneVariable{Key: symbol, Value: fdNode})
	return progn(ctx, newWorld, list)
}

func funCreateStringInputStream(ctx context.Context, w *World, list []Node) (Node, error) {
	s, ok := list[0].(String)
	if !ok {
		return nil, ErrExpectedString
	}
	return _Reader{Reader: bufio.NewReader(strings.NewReader(string(s)))}, nil
}
