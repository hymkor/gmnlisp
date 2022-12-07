package auto

import (
	"bufio"
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"strings"

	. "github.com/hymkor/gmnlisp"
)

func init() {
	ExportRange(Variables{
		NewSymbol("foreach"):          SpecialF(cmdForeach),
		NewSymbol("command"):          defCommand,
		NewSymbol("open"):             SpecialF(cmdOpen),
		NewSymbol("read-from-string"): &Function{C: 1, F: funReadFromString},
		NewSymbol("strcase"):          &Function{C: 1, F: funStrCase},
	})
}

func funStrCase(ctx context.Context, w *World, argv []Node) (Node, error) {
	// from autolisp
	if str, ok := argv[0].(String); ok {
		return String(strings.ToUpper(str.String())), nil
	}
	return nil, ErrExpectedString
}

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

func (d _Dummy) String() string {
	return "(binary)"
}

func (d _Dummy) GoString() string {
	return "(binary)"
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
	fnameNode, n, err := w.ShiftAndEvalCar(ctx, n)
	if err != nil {
		return nil, err
	}
	fnameString, ok := fnameNode.(String)
	if !ok {
		return nil, fmt.Errorf("%w: %#v", ErrExpectedString, fnameString)
	}
	fname := fnameString.String()
	if IsNull(n) {
		return openAsRead(fname)
	}

	modeNode, n, err := w.ShiftAndEvalCar(ctx, n)
	if HasValue(n) {
		return nil, ErrTooManyArguments
	}
	modeString, ok := modeNode.(String)
	if !ok {
		return nil, fmt.Errorf("%w: %#v", ErrExpectedString, modeNode)
	}
	mode := modeString.String()

	var result Node
	switch mode {
	case "r":
		result, err = openAsRead(fname)
	case "w":
		result, err = openAsWrite(fname)
	default:
		return nil, fmt.Errorf("no such a option `%s`", mode)
	}
	if errors.Is(err, os.ErrNotExist) {
		return Null, nil
	}
	return result, err
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

func funReadFromString(_ context.Context, _ *World, args []Node) (Node, error) {
	// compatible with autolisp's (read)
	script, ok := args[0].(String)
	if !ok {
		return nil, ErrExpectedString
	}
	nodes, err := ReadAll(strings.NewReader(script.String()))
	if err != nil {
		return nil, err
	}
	if len(nodes) < 1 {
		return Null, nil
	}
	return nodes[0], nil
}

func cmdForeach(ctx context.Context, w *World, args Node) (Node, error) {
	// from autolisp
	var _symbol Node
	var err error

	_symbol, args, err = Shift(args)
	if err != nil {
		return nil, err
	}
	symbol, ok := _symbol.(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}

	var list Node
	var code Node
	list, code, err = w.ShiftAndEvalCar(ctx, args)
	if err != nil {
		return nil, err
	}

	var last Node
	for HasValue(list) {
		var value Node

		value, list, err = Shift(list)
		if err != nil {
			return nil, err
		}
		if err := w.Set(symbol, value); err != nil {
			return nil, err
		}

		last, err = Progn(ctx, w, code)
		if err != nil {
			return nil, err
		}
	}
	return last, nil
}
