package gmnlisp

import (
	"errors"
	"fmt"
	"io"
	"strings"
)

var ErrDevisionByZero = errors.New("Devision by zeor")

type Node interface {
	Null() bool
	Eval() (Node, error)
	Equals(Node) bool
	PrintTo(io.Writer)
	PrincTo(io.Writer)
}

func Node2String(node Node) string {
	if node == nil {
		return "()"
	}
	var buffer strings.Builder
	node.PrintTo(&buffer)
	return buffer.String()
}

type TrueType struct{}

func (TrueType) PrintTo(w io.Writer) {
	io.WriteString(w, "T")
}

func (TrueType) PrincTo(w io.Writer) {
	io.WriteString(w, "T")
}

func (TrueType) Null() bool {
	return false
}

func (t TrueType) Eval() (Node, error) {
	return t, nil
}

var TrueValue = TrueType{}

func (TrueType) Equals(n Node) bool {
	_, ok := n.(TrueType)
	return ok
}

type Null struct{}

func (Null) PrintTo(w io.Writer) {
	io.WriteString(w, "nil")
}

func (Null) PrincTo(w io.Writer) {
	io.WriteString(w, "nil")
}

func (Null) Null() bool {
	return true
}

func (this Null) Eval() (Node, error) {
	return this, nil // errors.New("Null can not be evaluate.")
}

func (this Null) Equals(n Node) bool {
	if n == nil {
		return true
	}
	_, ok := n.(Null)
	return ok
}

var NullValue = Null{}

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

func (this NodeString) Eval() (Node, error) {
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
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, Node2String(n))
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

func (this NodeSymbol) Eval() (Node, error) {
	name := string(this)
	if value, ok := globals[name]; ok {
		return value, nil
	}
	return nil, fmt.Errorf("variable `%s` unbound", name)
}

func (this NodeSymbol) Equals(n Node) bool {
	ns, ok := n.(NodeSymbol)
	return ok && this == ns
}
