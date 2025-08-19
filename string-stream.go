package gmnlisp

import (
	"context"
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
