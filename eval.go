package gmnlisp

import (
	"errors"
	"fmt"
	"io"
)

type Callable interface {
	Node
	Call(Node) (Node, error)
}

type Function func(Node) (Node, error)

func (Function) PrintTo(w io.Writer) {
	io.WriteString(w, "buildin function")
}

func (Function) PrincTo(w io.Writer) {
	io.WriteString(w, "buildin function")
}

func (Function) Null() bool {
	return false
}

func (f Function) Eval() (Node, error) {
	return f, nil
}

func (f Function) Equals(n Node) bool {
	return false
}

func (f Function) Call(n Node) (Node, error) {
	return f(n)
}

var ErrExpectedFunction = errors.New("expected function")

func (this *Cons) Eval() (Node, error) {
	first := this.Car
	if p, ok := first.(*Cons); ok {
		var err error
		first, err = p.Eval()
		if err != nil {
			return nil, err
		}
	}
	if f, ok := first.(Callable); ok {
		return f.Call(this.Cdr)
	}
	symbol, ok := first.(NodeSymbol)
	if !ok {
		return nil, fmt.Errorf("cons: %w", ErrExpectedFunction)
	}
	value, err := symbol.Eval()
	if err != nil {
		return nil, err
	}
	function, ok := value.(Callable)
	if !ok {
		return nil, fmt.Errorf("%s: %w", string(symbol), ErrExpectedFunction)
	}
	rv, err := function.Call(this.Cdr)
	if err != nil {
		return rv, fmt.Errorf("%s: %w", string(symbol), err)
	}
	return rv, nil
}
