package gommon

import (
	"fmt"
	"io"
	"strings"
)

type Node interface {
	io.WriterTo
	Null() bool
	Eval() (Node, error)
	Equals(Node) bool
}

func Node2String(node Node) string {
	if node == nil {
		return "()"
	}
	var buffer strings.Builder
	node.WriteTo(&buffer)
	return buffer.String()
}

type Null struct{}

func (this Null) WriteTo(w io.Writer) (int64, error) {
	n, err := fmt.Fprint(w, "()")
	return int64(n), err
}

func (this Null) Null() bool {
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

type NodeString string

func (this NodeString) WriteTo(w io.Writer) (int64, error) {
	n, err := fmt.Fprintf(w, "\"%s\"", string(this))
	return int64(n), err
}

func (this NodeString) Null() bool {
	return false
}

func (this NodeString) Eval() (Node, error) {
	return this, nil // errors.New("String can not be evaluate.")
}

func (this NodeString) Equals(n Node) bool {
	ns, ok := n.(NodeString)
	return ok && this == ns
}

type NodeSymbol string

func (this NodeSymbol) WriteTo(w io.Writer) (int64, error) {
	n, err := io.WriteString(w, string(this))
	return int64(n), err
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

type NodeInteger int64

func (this NodeInteger) WriteTo(w io.Writer) (int64, error) {
	n, err := fmt.Fprintf(w, "%d", int64(this))
	return int64(n), err
}

func (this NodeInteger) Null() bool {
	return false
}

func (this NodeInteger) Eval() (Node, error) {
	return this, nil // errors.New("Integer can not be evaluate.")
}

func (this NodeInteger) Equals(n Node) bool {
	ni, ok := n.(NodeInteger)
	return ok && this == ni
}
