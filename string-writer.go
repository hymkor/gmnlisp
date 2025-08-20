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

type stringWriter struct {
	strings.Builder
}

func (S *stringWriter) String() string {
	if S == nil {
		return ""
	}
	return S.Builder.String()
}

func (S *stringWriter) Column() int {
	s := S.String()
	pos := strings.LastIndexByte(s, '\n')
	if pos >= 0 {
		return len(s) - pos - 1
	}
	return len(s)
}

func (S *stringWriter) Add(ctx context.Context, w *World, n Node) error {
	r, err := ExpectClass[Rune](ctx, w, n)
	if err != nil {
		return err
	}
	S.WriteRune(rune(r))
	return nil
}

func (S *stringWriter) Close() error {
	S.Reset()
	return nil
}

func (S *stringWriter) Sequence() Node {
	return String(S.String())
}

func (S stringWriter) GoString() string {
	return strconv.Quote(S.String())
}

var stringBuilderClass = &BuiltInClass{
	name: NewSymbol("<string-builder>"),
	instanceP: func(value Node) bool {
		_, ok := value.(*stringWriter)
		return ok
	},
	create: func() Node {
		return &stringWriter{}
	},
	super: []Class{ObjectClass, streamClass},
}

func (*stringWriter) ClassOf() Class {
	return stringBuilderClass
}

func (t *stringWriter) Equals(other Node, _ EqlMode) bool {
	o, ok := other.(*stringWriter)
	if !ok {
		return false
	}
	return t.String() == o.String()
}

func (t *stringWriter) RawWriter() io.Writer {
	return &t.Builder
}

func funCreateStringOutputStream(ctx context.Context, w *World) (Node, error) {
	return &stringWriter{}, nil
}

func funGetOutputStreamString(ctx context.Context, w *World, arg Node) (Node, error) {
	stringer, err := ExpectClass[*stringWriter](ctx, w, arg)
	if err != nil {
		return nil, err
	}
	result := String(stringer.String())
	stringer.Reset()
	return result, nil
}
