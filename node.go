package gmnlisp

import (
	"fmt"
	"io"
	"strings"
)

type EqlMode int

const (
	EQUAL EqlMode = iota
	EQUALP
)

type Node interface {
	Eval(*World) (Node, error)
	Equals(Node, EqlMode) bool
	PrintTo(io.Writer)
}

func toString(node Node) string {
	if node == nil {
		return "()"
	}
	var buffer strings.Builder
	node.PrintTo(&buffer)
	return buffer.String()
}

type _TrueType struct{}

func (_TrueType) PrintTo(w io.Writer) {
	io.WriteString(w, "T")
}

func (t _TrueType) Eval(*World) (Node, error) {
	return t, nil
}

var True Node = _TrueType{}

func (_TrueType) Equals(n Node, m EqlMode) bool {
	_, ok := n.(_TrueType)
	return ok
}

type _NullType struct{}

func (_NullType) PrintTo(w io.Writer) {
	io.WriteString(w, "nil")
}

func (nt _NullType) Eval(*World) (Node, error) {
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

func (s String) PrintTo(w io.Writer) {
	fmt.Fprintf(w, "\"%s\"", string(s))
}

func (s String) PrincTo(w io.Writer) {
	io.WriteString(w, string(s))
}

func (s String) Eval(*World) (Node, error) {
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

func (s Symbol) PrintTo(w io.Writer) {
	io.WriteString(w, string(s))
}

func (s Symbol) Eval(w *World) (Node, error) {
	return w.Get(string(s))
}

func (s Symbol) Equals(n Node, m EqlMode) bool {
	ns, ok := n.(Symbol)
	if m == EQUALP {
		return ok && strings.EqualFold(string(s), string(ns))
	} else {
		return ok && s == ns
	}
}
