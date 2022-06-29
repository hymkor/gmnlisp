package gommon

import (
	"errors"
	"fmt"
	"io"
)

type Node interface {
	io.WriterTo
	Null() bool
	Eval() (Node, error)
}

type Null struct{}

func (this Null) WriteTo(w io.Writer) (int64, error) {
	n, err := fmt.Fprint(w, "<nil>")
	return int64(n), err
}

func (this Null) Null() bool {
	return true
}

func (this Null) Eval() (Node, error) {
	return this, errors.New("Null can not be evaluate.")
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
	return this, errors.New("String can not be evaluate.")
}

type NodeSymbol string

func (this NodeSymbol) WriteTo(w io.Writer) (int64, error) {
	n, err := fmt.Fprintf(w, "{%s}", string(this))
	return int64(n), err
}

func (this NodeSymbol) Null() bool {
	return false
}

func (this NodeSymbol) Eval() (Node, error) {
	return this, errors.New("Symbol can not be evaluate.")
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
	return this, errors.New("Integer can not be evaluate.")
}
