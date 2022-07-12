package gmnlisp

import (
	"errors"
	"fmt"
	"io"
)

type _Callable interface {
	Node
	Call(*World, Node) (Node, error)
}

type Function func(*World, Node) (Node, error)

func (Function) PrintTo(w io.Writer) {
	io.WriteString(w, "buildin function")
}

func (Function) PrincTo(w io.Writer) {
	io.WriteString(w, "buildin function")
}

func (f Function) Eval(_ *World) (Node, error) {
	return f, nil
}

func (f Function) Equals(n Node) bool {
	return false
}

func (f Function) EqualP(n Node) bool {
	return false
}

func (f Function) Call(w *World, n Node) (Node, error) {
	return f(w, n)
}

var ErrExpectedFunction = errors.New("expected function")

func (cons *Cons) Eval(w *World) (Node, error) {
	first := cons.Car
	if p, ok := first.(*Cons); ok {
		var err error
		first, err = p.Eval(w)
		if err != nil {
			return nil, err
		}
	}
	if f, ok := first.(_Callable); ok {
		return f.Call(w, cons.Cdr)
	}
	symbol, ok := first.(Symbol)
	if !ok {
		return nil, fmt.Errorf("cons: %w", ErrExpectedFunction)
	}
	value, err := symbol.Eval(w)
	if err != nil {
		return nil, err
	}
	function, ok := value.(_Callable)
	if !ok {
		return nil, fmt.Errorf("%s: %w", string(symbol), ErrExpectedFunction)
	}
	rv, err := function.Call(w, cons.Cdr)
	if err != nil {
		return rv, fmt.Errorf("%s: %w", string(symbol), err)
	}
	return rv, nil
}
