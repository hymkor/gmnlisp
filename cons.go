package gommon

import (
	"io"
)

type Cons struct {
	Car Node
	Cdr Node
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

func (this *Cons) WriteTo(w io.Writer) (int64, error) {
	var n int64
	if err := write(&n, w, "("); err != nil {
		return n, err
	}

	m, err := this.Car.WriteTo(w)
	n += m
	if err != nil {
		return n, err
	}

	if !IsNull(this.Cdr) {
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
	err = write(&n, w, ")")
	return n, nil
}
