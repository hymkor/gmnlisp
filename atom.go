package gmnlisp

import (
	"context"
	"fmt"
	"io"
	"strings"
)

type _TrueType struct{}

func (_TrueType) PrintTo(w io.Writer, m PrintMode) {
	io.WriteString(w, "T")
}

func (t _TrueType) Eval(context.Context, *World) (Node, error) {
	return t, nil
}

var True Node = _TrueType{}

func (_TrueType) Equals(n Node, m EqlMode) bool {
	_, ok := n.(_TrueType)
	return ok
}

type _NullType struct{}

func (_NullType) PrintTo(w io.Writer, m PrintMode) {
	io.WriteString(w, "nil")
}

func (nt _NullType) Eval(context.Context, *World) (Node, error) {
	return nt, nil
}

func (nt _NullType) Equals(n Node, m EqlMode) bool {
	if n == nil {
		return true
	}
	_, ok := n.(_NullType)
	return ok
}

var Null Node = _NullType{}

type String string

var unescapeSequenceReplacer = strings.NewReplacer(
	"\n", "\\n",
	"\r", "\\r",
	"\t", "\\t",
	"\b", "\\b",
	"\"", "\\\"",
)

func (s String) PrintTo(w io.Writer, m PrintMode) {
	if m == PRINC {
		io.WriteString(w, string(s))
	} else {
		fmt.Fprintf(w, `"%s"`, unescapeSequenceReplacer.Replace(string(s)))
	}
}

func (s String) Eval(context.Context, *World) (Node, error) {
	return s, nil // errors.New("String can not be evaluate.")
}

func (s String) Equals(n Node, m EqlMode) bool {
	ns, ok := n.(String)
	if m == EQUALP {
		return ok && strings.EqualFold(string(s), string(ns))
	} else {
		return ok && s == ns
	}
}

func (s String) Add(n Node) (Node, error) {
	if value, ok := n.(String); ok {
		return s + value, nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n))
}

func (s String) LessThan(n Node) (bool, error) {
	if ns, ok := n.(String); ok {
		return s < ns, nil
	}
	return false, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n))
}

type Symbol string

func (s Symbol) PrintTo(w io.Writer, m PrintMode) {
	io.WriteString(w, string(s))
}

func (s Symbol) Eval(_ context.Context, w *World) (Node, error) {
	return w.Get(s)
}

func (s Symbol) Equals(n Node, m EqlMode) bool {
	ns, ok := n.(Symbol)
	if m == EQUALP {
		return ok && strings.EqualFold(string(s), string(ns))
	} else {
		return ok && s == ns
	}
}

type Rune rune

func (r Rune) PrintTo(w io.Writer, m PrintMode) {
	if m == PRINT {
		fmt.Fprintf(w, "#\\%c", rune(r))
	} else {
		fmt.Fprintf(w, "%c", rune(r))
	}
}

func (r Rune) Eval(_ context.Context, w *World) (Node, error) {
	return r, nil
}

func (r Rune) Equals(n Node, m EqlMode) bool {
	if value, ok := n.(Rune); ok {
		return r == value
	}
	if m == EQUAL {
		return false
	}
	if value, ok := n.(String); ok {
		return len(string(value)) == 1 && string(r) == string(value)
	}
	return false
}
