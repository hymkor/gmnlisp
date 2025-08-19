package gmnlisp

import (
	"bufio"
	"context"
	"io"
	"os"
	"strconv"
	"strings"
)

var outputStreamClass = &BuiltInClass{
	name: NewSymbol("<output-stream>"),
	instanceP: func(value Node) bool {
		_, ok := value.(*outputStream)
		return ok
	},
	create: func() Node {
		return &outputStream{
			w:    bufio.NewWriter(os.Stderr),
			file: os.Stderr,
		}
	},
	super: []Class{ObjectClass, streamClass},
}

type StringBuilder struct {
	strings.Builder
}

func (S *StringBuilder) String() string {
	if S == nil {
		return ""
	}
	return S.Builder.String()
}

func (S *StringBuilder) Column() int {
	s := S.String()
	pos := strings.LastIndexByte(s, '\n')
	if pos >= 0 {
		return len(s) - pos - 1
	}
	return len(s)
}

func (S *StringBuilder) Add(ctx context.Context, w *World, n Node) error {
	r, err := ExpectClass[Rune](ctx, w, n)
	if err != nil {
		return err
	}
	S.WriteRune(rune(r))
	return nil
}

func (S *StringBuilder) Close() error {
	S.Reset()
	return nil
}

func (S *StringBuilder) Sequence() Node {
	return String(S.String())
}

func (S StringBuilder) GoString() string {
	return strconv.Quote(S.String())
}

var stringBuilderClass = &BuiltInClass{
	name: NewSymbol("<string-builder>"),
	instanceP: func(value Node) bool {
		_, ok := value.(*StringBuilder)
		return ok
	},
	create: func() Node {
		return &StringBuilder{}
	},
	super: []Class{ObjectClass, streamClass},
}

func (*StringBuilder) ClassOf() Class {
	return stringBuilderClass
}

func (t *StringBuilder) Equals(other Node, _ EqlMode) bool {
	o, ok := other.(*StringBuilder)
	if !ok {
		return false
	}
	return t.String() == o.String()
}

func (t *StringBuilder) RawWriter() io.Writer {
	return &t.Builder
}

func funCreateStringOutputStream(ctx context.Context, w *World) (Node, error) {
	return &StringBuilder{}, nil
}

func funGetOutputStreamString(ctx context.Context, w *World, arg Node) (Node, error) {
	stringer, err := ExpectClass[*StringBuilder](ctx, w, arg)
	if err != nil {
		return nil, err
	}
	result := String(stringer.String())
	stringer.Reset()
	return result, nil
}
