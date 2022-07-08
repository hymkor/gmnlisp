package gommon

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

func (Function) WriteTo(w io.Writer) (int64, error) {
	return toInt64(io.WriteString(w, "buildin function"))
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
	_name, ok := first.(NodeSymbol)
	if !ok {
		return nil, fmt.Errorf("cons: %w", ErrExpectedFunction)
	}
	name := string(_name)
	val, ok := globals[name]
	if !ok {
		return nil, fmt.Errorf("%s: Not found", name)
	}
	_fn, ok := val.(Callable)
	if !ok {
		return nil, fmt.Errorf("%s: Not Callable Object", name)
	}
	return _fn.Call(this.Cdr)
}
