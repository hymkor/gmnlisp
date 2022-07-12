package gmnlisp

import (
	"errors"
	"fmt"
	"io"
)

type _Callable interface {
	Node
	Call(*Instance, Node) (Node, error)
}

type Function func(*Instance, Node) (Node, error)

func (Function) PrintTo(w io.Writer) {
	io.WriteString(w, "buildin function")
}

func (Function) PrincTo(w io.Writer) {
	io.WriteString(w, "buildin function")
}

func (Function) IsNull() bool {
	return false
}

func (f Function) Eval(_ *Instance) (Node, error) {
	return f, nil
}

func (f Function) Equals(n Node) bool {
	return false
}

func (f Function) Call(ins *Instance, n Node) (Node, error) {
	return f(ins, n)
}

var ErrExpectedFunction = errors.New("expected function")

func (cons *Cons) Eval(ins *Instance) (Node, error) {
	first := cons.Car
	if p, ok := first.(*Cons); ok {
		var err error
		first, err = p.Eval(ins)
		if err != nil {
			return nil, err
		}
	}
	if f, ok := first.(_Callable); ok {
		return f.Call(ins, cons.Cdr)
	}
	symbol, ok := first.(Symbol)
	if !ok {
		return nil, fmt.Errorf("cons: %w", ErrExpectedFunction)
	}
	value, err := symbol.Eval(ins)
	if err != nil {
		return nil, err
	}
	function, ok := value.(_Callable)
	if !ok {
		return nil, fmt.Errorf("%s: %w", string(symbol), ErrExpectedFunction)
	}
	rv, err := function.Call(ins, cons.Cdr)
	if err != nil {
		return rv, fmt.Errorf("%s: %w", string(symbol), err)
	}
	return rv, nil
}
