package gmnlisp

import (
	"context"
	"fmt"
	"io"
)

type Cons struct {
	Car Node
	Cdr Node
}

func (cons *Cons) firstAndRest() (Node, Node, bool, func(Node) error) {
	car := cons.Car
	if car == nil {
		car = Null
	}
	cdr := cons.Cdr
	if cdr == nil {
		cdr = Null
	}
	return car, cdr, true, func(value Node) error {
		cons.Car = value
		return nil
	}
}

func (cons *Cons) GetCar() Node {
	if cons.Car == nil {
		return Null
	}
	return cons.Car
}

func (cons *Cons) GetCdr() Node {
	if cons.Cdr == nil {
		return Null
	}
	return cons.Cdr
}

func (cons *Cons) isTailNull() bool {
	if IsNull(cons.Cdr) {
		return true
	} else if next, ok := cons.Cdr.(*Cons); ok {
		return next.isTailNull()
	} else {
		return false
	}
}

func (cons *Cons) writeToWithoutKakko(w io.Writer, m PrintMode) {
	if IsNull(cons.Car) {
		io.WriteString(w, "()")
	} else {
		cons.Car.PrintTo(w, m)
	}

	if HasValue(cons.Cdr) {
		if cons.isTailNull() {
			// output as ( X Y Z ...)

			for p, ok := cons.Cdr.(*Cons); ok && HasValue(p); p, ok = p.Cdr.(*Cons) {
				io.WriteString(w, " ")
				p.Car.PrintTo(w, m)
			}
		} else {
			// output as ( X . Y )

			io.WriteString(w, " . ")
			cons.GetCdr().PrintTo(w, m)
		}
	}
}

func (cons *Cons) PrintTo(w io.Writer, m PrintMode) {
	io.WriteString(w, "(")
	cons.writeToWithoutKakko(w, m)
	io.WriteString(w, ")")
}

func (cons *Cons) Equals(n Node, m EqlMode) bool {
	value, ok := n.(*Cons)
	if !ok {
		return false
	}
	return cons.GetCar().Equals(value.Car, m) &&
		cons.GetCdr().Equals(value.Cdr, m)
}

func (cons *Cons) Eval(ctx context.Context, w *World) (Node, error) {
	first := cons.Car
	if p, ok := first.(*Cons); ok {
		var err error
		first, err = p.Eval(ctx, w)
		if err != nil {
			return nil, err
		}
	}
	if f, ok := first.(_Callable); ok {
		return f.Call(ctx, w, cons.Cdr)
	}
	symbol, ok := first.(Symbol)
	if !ok {
		return nil, fmt.Errorf("cons: %w", ErrExpectedFunction)
	}
	value, err := symbol.Eval(ctx, w)
	if err != nil {
		return nil, err
	}
	function, ok := value.(_Callable)
	if !ok {
		return nil, fmt.Errorf("%s: %w", string(symbol), ErrExpectedFunction)
	}
	rv, err := function.Call(ctx, w, cons.Cdr)
	if err != nil {
		return rv, fmt.Errorf("%s: %w", string(symbol), err)
	}
	return rv, nil
}
