package gmnlisp

import (
	"context"
	"fmt"
	"io"
	"strings"
)

type Cons struct {
	Car Node
	Cdr Node
}

var consClass = registerNewAbstractClass[*Cons]("<cons>")

func (*Cons) ClassOf() Class {
	return consClass
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

func tryPrintTo(w io.Writer, node Node, m PrintMode) (int, error) {
	if p, ok := node.(interface {
		PrintTo(io.Writer, PrintMode) (int, error)
	}); ok {
		return p.PrintTo(w, m)
	}
	if m == PRINT {
		if p, ok := node.(fmt.GoStringer); ok {
			return io.WriteString(w, p.GoString())
		}
	}
	return io.WriteString(w, node.String())
}

func (cons *Cons) writeToWithoutKakko(w io.Writer, m PrintMode) (int, error) {
	var wc writeCounter
	var lastCar Node = cons.Car
	if IsNone(cons.Car) {
		if wc.Try(io.WriteString(w, "NIL")) {
			return wc.Result()
		}
	} else {
		if wc.Try(tryPrintTo(w, cons.Car, m)) {
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
				if IsNull(p.Car) {
					if wc.Try(io.WriteString(w, "NIL")) {
						return wc.Result()
					}
				} else if wc.Try(tryPrintTo(w, p.Car, m)) {
					return wc.Result()
				}
				lastCar = p.Car
			}
		} else {
			// output as ( X . Y )
			if wc.Try(io.WriteString(w, " . ")) {
				return wc.Result()
			}
			if wc.Try(tryPrintTo(w, cons.getCdr(), m)) {
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
			wc.Try(tryPrintTo(w, cdr.Car, m))
			return wc.Result()
		}
	}
	if cons.Car == backQuoteSymbol {
		if cdr, ok := cons.Cdr.(*Cons); ok && IsSome(cdr.Car) && IsNone(cdr.Cdr) {
			if wc.Try(w.Write([]byte{'`'})) {
				return wc.Result()
			}
			wc.Try(tryPrintTo(w, cdr.Car, m))
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

func (t Cons) String() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINC)
	return buffer.String()
}

func (t Cons) GoString() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINT)
	return buffer.String()
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
	var rc Node
	var err error
	symbol, ok := cons.Car.(Symbol)
	if !ok {
		f, _err := w.Eval(ctx, cons.Car)
		if _err != nil {
			return nil, _err
		}
		function, ok := f.(FunctionRef)
		if !ok {
			rc, err = callHandler[FunctionRef](ctx, w, true, &_UndefinedEntity{
				name:  NewSymbol(f.String()),
				space: symFunction,
			})
		} else {
			rc, err = function.value.Call(ctx, w, cons.Cdr)
		}
	} else {
		function, _err := w.GetFunc(symbol)
		if _err != nil {
			return nil, _err
		}
		rc, err = function.Call(ctx, w, cons.Cdr)
	}
	if err != nil {
		return nil, fmt.Errorf("%w\n\tat %v", err, symbol)
	}
	return rc, nil
}
