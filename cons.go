package gmnlisp

import (
	"io"
)

type Cons struct {
	Car Node
	Cdr Node
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

func HasValue(node Node) bool {
	if node == nil {
		return false
	}
	_, ok := node.(_NullType)
	return !ok
}

func IsNull(node Node) bool {
	if node == nil {
		return true
	}
	_, ok := node.(_NullType)
	return ok
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

func (cons *Cons) writeToWithoutKakko(w io.Writer, rich bool) {
	if IsNull(cons.Car) {
		io.WriteString(w, "()")
	} else if rich {
		cons.Car.PrintTo(w)
	} else {
		cons.Car.PrincTo(w)
	}

	if HasValue(cons.Cdr) {
		if cons.isTailNull() {
			// output as ( X Y Z ...)

			for p, ok := cons.Cdr.(*Cons); ok && HasValue(p); p, ok = p.Cdr.(*Cons) {
				io.WriteString(w, " ")
				if rich {
					p.Car.PrintTo(w)
				} else {
					p.Car.PrincTo(w)
				}
			}
		} else {
			// output as ( X . Y )

			io.WriteString(w, " . ")
			if rich {
				cons.GetCdr().PrintTo(w)
			} else {
				cons.GetCdr().PrincTo(w)
			}
		}
	}
}

func (cons *Cons) PrintTo(w io.Writer) {
	io.WriteString(w, "(")
	cons.writeToWithoutKakko(w, true)
	io.WriteString(w, ")")
}

func (cons *Cons) PrincTo(w io.Writer) {
	io.WriteString(w, "(")
	cons.writeToWithoutKakko(w, false)
	io.WriteString(w, ")")
}

func (cons *Cons) Equals(n Node) bool {
	value, ok := n.(*Cons)
	if !ok {
		return false
	}
	return cons.GetCar().Equals(value.Car) &&
		cons.GetCdr().Equals(value.Cdr)
}

func (cons *Cons) EqualP(n Node) bool {
	_n, ok := n.(*Cons)
	if !ok {
		return false
	}
	if !cons.GetCar().EqualP(_n.GetCar()) {
		return false
	}
	return cons.GetCdr().EqualP(_n.GetCdr())
}
