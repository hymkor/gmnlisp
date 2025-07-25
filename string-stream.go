package gmnlisp

import (
	"bufio"
	"context"
	"io"
	"os"
	"strings"
)

type StringReader struct {
	*strings.Reader
}

var stringReaderClass = &BuiltInClass{
	name: NewSymbol("<string-stream-reader>"),
	instanceP: func(v Node) bool {
		_, ok := v.(StringReader)
		return ok
	},
	create: func() Node {
		return StringReader{Reader: strings.NewReader("")}
	},
	super: []Class{ObjectClass, streamClass},
}

func (sr StringReader) QueryStreamReady() (Node, error) {
	if sr.Reader.Len() <= 0 {
		return Null, nil
	}
	return True, nil
}

func (sr StringReader) ClassOf() Class {
	return stringReaderClass
}

func (sr StringReader) Equals(other Node, _ EqlMode) bool {
	o, ok := other.(StringReader)
	if !ok {
		return false
	}
	return o.Reader == sr.Reader
}

func (sr StringReader) String() string {
	return "<stream> for string"
}

func (sr StringReader) Close() error {
	return nil
}

func funCreateStringInputStream(ctx context.Context, w *World, arg Node) (Node, error) {
	s, err := ExpectClass[String](ctx, w, arg)
	if err != nil {
		return nil, err
	}
	return StringReader{Reader: strings.NewReader(s.String())}, nil
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
