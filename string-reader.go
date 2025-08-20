package gmnlisp

import (
	"context"
	"strings"
)

type stringReader struct {
	*strings.Reader
}

var stringReaderClass = &BuiltInClass{
	name: NewSymbol("<string-stream-reader>"),
	instanceP: func(v Node) bool {
		_, ok := v.(stringReader)
		return ok
	},
	create: func() Node {
		return stringReader{Reader: strings.NewReader("")}
	},
	super: []Class{ObjectClass, streamClass},
}

func (sr stringReader) QueryStreamReady() (Node, error) {
	if sr.Reader.Len() <= 0 {
		return Null, nil
	}
	return True, nil
}

func (sr stringReader) ClassOf() Class {
	return stringReaderClass
}

func (sr stringReader) Equals(other Node, _ EqlMode) bool {
	o, ok := other.(stringReader)
	if !ok {
		return false
	}
	return o.Reader == sr.Reader
}

func (sr stringReader) String() string {
	return "<stream> for string"
}

func (sr stringReader) Close() error {
	return nil
}

func funCreateStringInputStream(ctx context.Context, w *World, arg Node) (Node, error) {
	s, err := ExpectClass[String](ctx, w, arg)
	if err != nil {
		return nil, err
	}
	return stringReader{Reader: strings.NewReader(s.String())}, nil
}
