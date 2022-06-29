package gommon

import (
	"errors"
	"fmt"
	"io"
	"regexp"
	"strconv"
	"strings"
)

type Node interface {
	io.WriterTo
	Null() bool
	Eval() (Node, error)
}

type Null struct{}

func (this Null) WriteTo(w io.Writer) (int64, error) {
	n, err := fmt.Fprint(w, "<nil>")
	return int64(n), err
}

func (this Null) Null() bool {
	return true
}

func (this Null) Eval() (Node, error) {
	return this, errors.New("Null can not be evaluate.")
}

type NodeString string

func (this NodeString) WriteTo(w io.Writer) (int64, error) {
	n, err := fmt.Fprintf(w, "\"%s\"", string(this))
	return int64(n), err
}

func (this NodeString) Null() bool {
	return false
}

func (this NodeString) Eval() (Node, error) {
	return this, errors.New("String can not be evaluate.")
}

type NodeSymbol string

func (this NodeSymbol) WriteTo(w io.Writer) (int64, error) {
	n, err := fmt.Fprintf(w, "{%s}", string(this))
	return int64(n), err
}

func (this NodeSymbol) Null() bool {
	return false
}

func (this NodeSymbol) Eval() (Node, error) {
	return this, errors.New("Symbol can not be evaluate.")
}

type NodeInteger int64

func (this NodeInteger) WriteTo(w io.Writer) (int64, error) {
	n, err := fmt.Fprintf(w, "%d", int64(this))
	return int64(n), err
}

func (this NodeInteger) Null() bool {
	return false
}

func (this NodeInteger) Eval() (Node, error) {
	return this, errors.New("Integer can not be evaluate.")
}

type Cons struct {
	Car Node
	Cdr Node
}

func (this *Cons) Null() bool {
	return false
}

var RxNumber = regexp.MustCompile("^[0-9]+$")

func readTokens(tokens []string) (Node, int) {
	if len(tokens) <= 0 {
		return &Null{}, 0
	}
	if tokens[0] == ")" {
		return &Null{}, 1
	}
	first := new(Cons)
	last := first
	i := 0
	for {
		if tokens[i] == "(" {
			i++
			car, n := readTokens(tokens[i:])
			last.Car = car
			i += n
		} else if RxNumber.MatchString(tokens[i]) {
			val, err := strconv.ParseInt(tokens[i], 10, 63)
			if err != nil {
				val = 0
			}
			last.Car = NodeInteger(val)
			i++
		} else {
			if strings.HasPrefix(tokens[i], "\"") {
				last.Car = NodeString(strings.Replace(tokens[i], "\"", "", -1))
			} else {
				last.Car = NodeSymbol(tokens[i])
			}
			i++
		}
		if i >= len(tokens) {
			last.Cdr = &Null{}
			return first, i
		}
		if tokens[i] == ")" {
			last.Cdr = &Null{}
			return first, i + 1
		}
		tmp := new(Cons)
		last.Cdr = tmp
		last = tmp
	}
}

func ReadTokens(tokens []string) Node {
	list, _ := readTokens(tokens)
	return list
}

func ReadString(s string) Node {
	return ReadTokens(StringToTokens(s))
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
