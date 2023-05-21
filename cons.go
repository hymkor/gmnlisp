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

type writeCounter struct {
	n   int
	err error
}

func (w *writeCounter) Try(n int, err error) bool {
	w.n += n
	w.err = err
	return err != nil
}

func (w *writeCounter) Result() (int, error) {
	return w.n, w.err
}

func (cons *Cons) writeToWithoutKakko(w io.Writer, m PrintMode) (int, error) {
	var wc writeCounter
	var lastCar Node = cons.Car
	if IsNone(cons.Car) {
		if wc.Try(io.WriteString(w, "nil")) {
			return wc.Result()
		}
	} else {
		if wc.Try(cons.Car.PrintTo(w, m)) {
			return wc.Result()
		}
	}

	if IsSome(cons.Cdr) {
		if cons.isTailNull() {
			// output as ( X Y Z ...)

			for p, ok := cons.Cdr.(*Cons); ok && IsSome(p); p, ok = p.Cdr.(*Cons) {
				if lastCar != commaSymbol {
					if wc.Try(io.WriteString(w, " ")) {
						return wc.Result()
					}
				}
				if wc.Try(p.Car.PrintTo(w, m)) {
					return wc.Result()
				}
				lastCar = p.Car
			}
		} else {
			// output as ( X . Y )
			if wc.Try(io.WriteString(w, " . ")) {
				return wc.Result()
			}
			if wc.Try(cons.getCdr().PrintTo(w, m)) {
				return wc.Result()
			}
		}
	}
	return wc.Result()
}

func (cons *Cons) PrintTo(w io.Writer, m PrintMode) (int, error) {
	var wc writeCounter
	if cons.Car == quoteSymbol {
		if cdr, ok := cons.Cdr.(*Cons); ok && IsSome(cdr.Car) && IsNone(cdr.Cdr) {
			if wc.Try(w.Write([]byte{'\''})) {
				return wc.Result()
			}
			wc.Try(cdr.Car.PrintTo(w, m))
			return wc.Result()
		}
	}
	if cons.Car == backQuoteSymbol {
		if cdr, ok := cons.Cdr.(*Cons); ok && IsSome(cdr.Car) && IsNone(cdr.Cdr) {
			if wc.Try(w.Write([]byte{'`'})) {
				return wc.Result()
			}
			wc.Try(cdr.Car.PrintTo(w, m))
			return wc.Result()
		}
	}
	if wc.Try(io.WriteString(w, "(")) ||
		wc.Try(cons.writeToWithoutKakko(w, m)) {
		return wc.Result()
	}
	wc.Try(io.WriteString(w, ")"))
	return wc.Result()
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
