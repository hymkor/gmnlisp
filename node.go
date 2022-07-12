package gmnlisp

import (
	"errors"
	"fmt"
	"io"
	"strings"
)

var (
	ErrDevisionByZero  = errors.New("Devision by zeor")
	ErrVariableUnbound = errors.New("Unbound variable")
)

type Node interface {
	Eval(*Instance) (Node, error)
	Equals(Node) bool
	PrintTo(io.Writer)
	PrincTo(io.Writer)
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

func (_TrueType) PrincTo(w io.Writer) {
	io.WriteString(w, "T")
}

func (t _TrueType) Eval(*Instance) (Node, error) {
	return t, nil
}

var True Node = _TrueType{}

func (_TrueType) Equals(n Node) bool {
	_, ok := n.(_TrueType)
	return ok
}

type _NullType struct{}

func (_NullType) PrintTo(w io.Writer) {
	io.WriteString(w, "nil")
}

func (_NullType) PrincTo(w io.Writer) {
	io.WriteString(w, "nil")
}

func (nt _NullType) Eval(*Instance) (Node, error) {
	return nt, nil
}

func (nt _NullType) Equals(n Node) bool {
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

func (s String) Eval(*Instance) (Node, error) {
	return s, nil // errors.New("String can not be evaluate.")
}

func (s String) Equals(n Node) bool {
	ns, ok := n.(String)
	return ok && s == ns
}

func (s String) Plus(n Node) (Node, error) {
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

func (s Symbol) PrincTo(w io.Writer) {
	io.WriteString(w, string(s))
}

func (s Symbol) Eval(ins *Instance) (Node, error) {
	name := string(s)
	if value, ok := ins.globals[name]; ok {
		return value, nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrVariableUnbound, name)
}

func (s Symbol) Equals(n Node) bool {
	ns, ok := n.(Symbol)
	return ok && s == ns
}
