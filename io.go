package gmnlisp

import (
	"bufio"
	"context"
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

type _ReadStringer interface {
	io.Reader
	io.RuneScanner
	ReadString(byte) (string, error)
}

type _StreamInput struct {
	reader   _ReadStringer
	eofFlag  bool
	eofValue Node
}

func newStreamInput(w *World, argv []Node) (*_StreamInput, error) {
	this := &_StreamInput{
		reader:   nil,
		eofFlag:  true,
		eofValue: Null,
	}
	switch len(argv) {
	default:
		return nil, ErrTooManyArguments
	case 3:
		this.eofValue = argv[2]
		fallthrough
	case 2:
		this.eofFlag = HasValue(argv[1])
		fallthrough
	case 1:
		var ok bool
		this.reader, ok = argv[0].(_ReadStringer)
		if !ok {
			return nil, fmt.Errorf("Expected Reader `%s`", ToString(argv[0], PRINT))
		}
	case 0:
		this.reader = w.Stdin()
	}
	return this, nil
}

var defReadLine = &Function{Max: 3, F: funReadLine}

func funReadLine(_ context.Context, w *World, argv []Node) (Node, error) {
	stream, err := newStreamInput(w, argv)
	if err != nil {
		return nil, err
	}
	s, err := stream.reader.ReadString('\n')
	if err == io.EOF {
		if stream.eofFlag {
			return Null, io.EOF
		}
		return stream.eofValue, nil
	}
	return String(chomp(s)), err
}

var defRead = &Function{Max: 3, F: funRead}

func funRead(_ context.Context, w *World, argv []Node) (Node, error) {
	stream, err := newStreamInput(w, argv)
	if err != nil {
		return nil, err
	}
	value, err := ReadNode(stream.reader)
	if err == io.EOF && !stream.eofFlag {
		return stream.eofValue, nil
	}
	return value, err
}

func funClose(_ context.Context, _ *World, argv []Node) (Node, error) {
	c, ok := argv[0].(io.Closer)
	if !ok {
		return nil, fmt.Errorf("Expected Closer `%s`", ToString(argv[0], PRINT))
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

	cmd := exec.CommandContext(ctx, argv[0], argv[1:]...)
	cmd.Stdout = w.Stdout()
	cmd.Stderr = w.Errout()
	cmd.Stdin = os.Stdin // w.Stdin()
	return Null, cmd.Run()
}

func cmdWithOpenFile(ctx context.Context, w *World, list Node) (Node, error) {
	var param Node
	var err error

	param, list, err = Shift(list)
	if err != nil {
		return nil, err
	}

	var symbolNode Node
	symbolNode, param, err = Shift(param)
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

	fname, ok := args[0].(StringTypes)
	if !ok {
		return nil, ErrExpectedString
	}
	var fdNode Node
	direction, ok := kwargs[":direction"]
	if !ok || direction == Keyword(":input") {
		fd, err := os.Open(fname.String())
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
		fd, err := os.Create(fname.String())
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
		return nil, fmt.Errorf("invalid :direction %s", ToString(direction, PRINT))
	}
	newWorld := w.Let(&Pair{Key: symbol, Value: fdNode})
	return progn(ctx, newWorld, list)
}

func funCreateStringInputStream(ctx context.Context, w *World, list []Node) (Node, error) {
	s, ok := list[0].(StringTypes)
	if !ok {
		return nil, ErrExpectedString
	}
	return _Reader{Reader: bufio.NewReader(strings.NewReader(s.String()))}, nil
}

func cmdCreateStringOutputStream(ctx context.Context, w *World, list Node) (Node, error) {
	type StringBuilder struct {
		_Dummy
		*strings.Builder
	}
	return &StringBuilder{Builder: &strings.Builder{}}, nil
}

func funGetOutputStreamString(ctx context.Context, w *World, list []Node) (Node, error) {
	stringer, ok := list[0].(fmt.Stringer) // expect StringBuilder
	if !ok {
		return nil, ErrNotSupportType
	}
	return String(stringer.String()), nil
}
