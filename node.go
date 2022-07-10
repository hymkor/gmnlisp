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
	IsNull() bool
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

func (_TrueType) IsNull() bool {
	return false
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

func (_NullType) IsNull() bool {
	return true
}

func (this _NullType) Eval(*Instance) (Node, error) {
	return this, nil // errors.New("IsNull can not be evaluate.")
}

func (this _NullType) Equals(n Node) bool {
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

func (String) IsNull() bool {
	return false
}

func (this String) Eval(*Instance) (Node, error) {
	return this, nil // errors.New("String can not be evaluate.")
}

func (this String) Equals(n Node) bool {
	ns, ok := n.(String)
	return ok && this == ns
}

func (this String) Plus(n Node) (Node, error) {
	if value, ok := n.(String); ok {
		return this + value, nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n))
}

type Symbol string

func (this Symbol) PrintTo(w io.Writer) {
	io.WriteString(w, string(this))
}

func (this Symbol) PrincTo(w io.Writer) {
	io.WriteString(w, string(this))
}

func (this Symbol) IsNull() bool {
	return false
}

func (this Symbol) Eval(instance *Instance) (Node, error) {
	name := string(this)
	if value, ok := instance.globals[name]; ok {
		return value, nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrVariableUnbound, name)
}

func (this Symbol) Equals(n Node) bool {
	ns, ok := n.(Symbol)
	return ok && this == ns
}
