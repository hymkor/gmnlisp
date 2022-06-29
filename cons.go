package gommon

import (
	"fmt"
	"io"
)

type Cons struct {
	Car Node
	Cdr Node
}

func (this *Cons) Null() bool {
	return false
}

func (this *Cons) WriteTo(w io.Writer) (int64, error) {
	var n int64
	m, err := fmt.Fprint(w, "( ")
	n += int64(m)
	if err != nil {
		return n, err
	}

	for !this.Null() {
		m, err := this.Car.WriteTo(w)
		n += m
		if err != nil {
			return n, err
		}
		p, ok := this.Cdr.(*Cons)
		if !ok {
			if this.Cdr.Null() {
				break
			}
			_m, err := fmt.Fprint(w, " . ")
			n += int64(_m)
			if err != nil {
				return n, err
			}
			m, err := this.Cdr.WriteTo(w)
			n += m
			if err != nil {
				return n, err
			}
		}
		this = p
		fmt.Fprint(w, " ")
	}
	m, err = fmt.Fprint(w, " )")
	n += int64(m)
	return n, nil
}
