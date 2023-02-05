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

func (cons *Cons) FirstAndRest() (Node, Node, bool) {
	car := cons.Car
	if car == nil {
		car = Null
	}
	cdr := cons.Cdr
	if cdr == nil {
		cdr = Null
	}
	return car, cdr, true
}

func (cons *Cons) getCar() Node {
	if cons.Car == nil {
		return Null
	}
	return cons.Car
}

func (cons *Cons) getCdr() Node {
	if cons.Cdr == nil {
		return Null
	}
	return cons.Cdr
}

func (cons *Cons) isTailNull() bool {
	if IsNone(cons.Cdr) {
		return true
	} else if next, ok := cons.Cdr.(*Cons); ok {
		return next.isTailNull()
	} else {
		return false
	}
}

func (cons *Cons) writeToWithoutKakko(w io.Writer, m PrintMode) (int, error) {
	siz := 0
	var lastCar Node = cons.Car
	if IsNone(cons.Car) {
		_siz, _ := io.WriteString(w, "nil")
		siz += _siz
	} else {
		_siz, _ := cons.Car.PrintTo(w, m)
		siz += _siz
	}

	if IsSome(cons.Cdr) {
		if cons.isTailNull() {
			// output as ( X Y Z ...)

			for p, ok := cons.Cdr.(*Cons); ok && IsSome(p); p, ok = p.Cdr.(*Cons) {
				if lastCar != commaSymbol {
					_siz, _ := io.WriteString(w, " ")
					siz += _siz
				}
				_siz, _ := p.Car.PrintTo(w, m)
				siz += _siz
				lastCar = p.Car
			}
		} else {
			// output as ( X . Y )

			_siz, _ := io.WriteString(w, " . ")
			siz += _siz
			_siz, _ = cons.getCdr().PrintTo(w, m)
			siz += _siz
		}
	}
	return siz, nil
}

func (cons *Cons) PrintTo(w io.Writer, m PrintMode) (int, error) {
	if cons.Car == quoteSymbol {
		if cdr, ok := cons.Cdr.(*Cons); ok && IsSome(cdr.Car) && IsNone(cdr.Cdr) {
			siz, _ := w.Write([]byte{'\''})
			_siz, _ := cdr.Car.PrintTo(w, m)
			siz += _siz
			return siz, nil
		}
	}
	if cons.Car == backQuoteSymbol {
		if cdr, ok := cons.Cdr.(*Cons); ok && IsSome(cdr.Car) && IsNone(cdr.Cdr) {
			siz, _ := w.Write([]byte{'`'})
			_siz, _ := cdr.Car.PrintTo(w, m)
			siz += _siz
			return siz, nil
		}
	}
	siz, _ := io.WriteString(w, "(")
	_siz, _ := cons.writeToWithoutKakko(w, m)
	siz += _siz
	_siz, _ = io.WriteString(w, ")")
	siz += _siz
	return siz, nil
}

func (cons *Cons) Equals(n Node, m EqlMode) bool {
	value, ok := n.(*Cons)
	if !ok {
		return false
	}
	if m == STRICT {
		return cons == value
	}
	return cons.getCar().Equals(value.Car, m) &&
		cons.getCdr().Equals(value.Cdr, m)
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
	if f, ok := first.(Callable); ok {
		rc, err := f.Call(ctx, w, cons.Cdr)
		if err != nil {
			return nil, fmt.Errorf("%w\n\tat %v", err, first)
		}
		return rc, nil
	}
	symbol, ok := first.(Symbol)
	if !ok {
		return nil, fmt.Errorf("%w: %#v", ErrExpectedSymbol, first)
	}
	value, err := symbol.Eval(ctx, w)
	if err != nil {
		return nil, err
	}
	function, ok := value.(Callable)
	if !ok {
		return nil, fmt.Errorf("%s: %w", symbol, ErrExpectedFunction)
	}
	rc, err := function.Call(ctx, w, cons.Cdr)
	if err != nil {
		return nil, fmt.Errorf("%w\n\tat %v", err, first)
	}
	return rc, nil
}
