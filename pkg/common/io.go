package common

import (
	"bufio"
	"context"
	"fmt"
	"io"
	"os"

	. "github.com/hymkor/gmnlisp"
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
	args, kwargs, err = ListToKwargs(ctx, w, param)
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
	return Progn(ctx, newWorld, list)
}
