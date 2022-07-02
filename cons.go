package gommon

import (
	"io"
)

type Cons struct {
	Car Node
	Cdr Node
}

func (c *Cons) GetCar() Node {
	if c.Car == nil {
		return &Null{}
	}
	return c.Car
}

func (c *Cons) GetCdr() Node {
	if c.Cdr == nil {
		return &Null{}
	}
	return c.Cdr
}

func (this *Cons) Null() bool {
	return false
}
func IsNull(node Node) bool {
	return node == nil || node.Null()
}

func write(n *int64, w io.Writer, s string) error {
	_n, err := io.WriteString(w, s)
	*n += int64(_n)
	return err
}

func (this *Cons) isTailNull() bool {
	if IsNull(this.Cdr) {
		return true
	} else if next, ok := this.Cdr.(*Cons); ok {
		return next.isTailNull()
	} else {
		return false
	}
}

func (this *Cons) WriteTo(w io.Writer) (int64, error) {
	var n int64
	if err := write(&n, w, "("); err != nil {
		return n, err
	}

	if IsNull(this.Car) {
		m, err := io.WriteString(w, "()")
		n += int64(m)
		if err != nil {
			return n, err
		}
	} else {
		m, err := this.Car.WriteTo(w)
		n += m
		if err != nil {
			return n, err
		}
	}

	if !IsNull(this.Cdr) {
		if this.isTailNull() {
			// output as ( X Y Z ...)

			for p, ok := this.Cdr.(*Cons); ok && !IsNull(p); p, ok = p.Cdr.(*Cons) {
				write(&n, w, " ")
				_n, err := p.Car.WriteTo(w)
				n += _n
				if err != nil {
					return n, err
				}
			}

		} else {
			// output as ( X . Y )
			if err := write(&n, w, " . "); err != nil {
				return n, err
			}
			m, err := this.Cdr.WriteTo(w)
			n += m
			if err != nil {
				return n, err
			}
		}
	}
	err := write(&n, w, ")")
	return n, err
}

func (this *Cons) Equals(n Node) bool {
	value, ok := n.(*Cons)
	if !ok {
		return false
	}
	return this.GetCar().Equals(value.Car) &&
		this.GetCdr().Equals(value.Cdr)
}
