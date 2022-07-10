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
	Null() bool
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

func (_TrueType) Null() bool {
	return false
}

func (t _TrueType) Eval(*Instance) (Node, error) {
	return t, nil
}

var TrueValue Node = _TrueType{}

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

func (_NullType) Null() bool {
	return true
}

func (this _NullType) Eval(*Instance) (Node, error) {
	return this, nil // errors.New("Null can not be evaluate.")
}

func (this _NullType) Equals(n Node) bool {
	if n == nil {
		return true
	}
	_, ok := n.(_NullType)
	return ok
}

var NullValue Node = _NullType{}

type NodeString string

func (s NodeString) PrintTo(w io.Writer) {
	fmt.Fprintf(w, "\"%s\"", string(s))
}

func (s NodeString) PrincTo(w io.Writer) {
	io.WriteString(w, string(s))
}

func (NodeString) Null() bool {
	return false
}

func (this NodeString) Eval(*Instance) (Node, error) {
	return this, nil // errors.New("String can not be evaluate.")
}

func (this NodeString) Equals(n Node) bool {
	ns, ok := n.(NodeString)
	return ok && this == ns
}

func (this NodeString) Plus(n Node) (Node, error) {
	if value, ok := n.(NodeString); ok {
		return this + value, nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n))
}

type NodeSymbol string

func (this NodeSymbol) PrintTo(w io.Writer) {
	io.WriteString(w, string(this))
}

func (this NodeSymbol) PrincTo(w io.Writer) {
	io.WriteString(w, string(this))
}

func (this NodeSymbol) Null() bool {
	return false
}

func (this NodeSymbol) Eval(instance *Instance) (Node, error) {
	name := string(this)
	if value, ok := instance.globals[name]; ok {
		return value, nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrVariableUnbound, name)
}

func (this NodeSymbol) Equals(n Node) bool {
	ns, ok := n.(NodeSymbol)
	return ok && this == ns
}
