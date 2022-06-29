package gommon

import (
	"errors"
	"fmt"
	"io"
	"regexp"
	"strconv"
	"strings"
)

type Atom interface {
	io.WriterTo
	Null() bool
	Eval() (Atom, error)
}

type Null struct{}

func (this Null) WriteTo(w io.Writer) (int64, error) {
	n, err := fmt.Fprint(w, "<nil>")
	return int64(n), err
}

func (this Null) Null() bool {
	return true
}

func (this Null) Eval() (Atom, error) {
	return this, errors.New("Null can not be evaluate.")
}

type AtomString string

func (this AtomString) WriteTo(w io.Writer) (int64, error) {
	n, err := fmt.Fprintf(w, "\"%s\"", string(this))
	return int64(n), err
}

func (this AtomString) Null() bool {
	return false
}

func (this AtomString) Eval() (Atom, error) {
	return this, errors.New("String can not be evaluate.")
}

type AtomSymbol string

func (this AtomSymbol) WriteTo(w io.Writer) (int64, error) {
	n, err := fmt.Fprintf(w, "{%s}", string(this))
	return int64(n), err
}

func (this AtomSymbol) Null() bool {
	return false
}

func (this AtomSymbol) Eval() (Atom, error) {
	return this, errors.New("Symbol can not be evaluate.")
}

type AtomInteger int64

func (this AtomInteger) WriteTo(w io.Writer) (int64, error) {
	n, err := fmt.Fprintf(w, "%d", int64(this))
	return int64(n), err
}

func (this AtomInteger) Null() bool {
	return false
}

func (this AtomInteger) Eval() (Atom, error) {
	return this, errors.New("Integer can not be evaluate.")
}

type Cons struct {
	Car Atom
	Cdr Atom
}

func (this *Cons) Null() bool {
	return false
}

var RxNumber = regexp.MustCompile("^[0-9]+$")

func readTokens(tokens []string) (Atom, int) {
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
			last.Car = AtomInteger(val)
			i++
		} else {
			if strings.HasPrefix(tokens[i], "\"") {
				last.Car = AtomString(strings.Replace(tokens[i], "\"", "", -1))
			} else {
				last.Car = AtomSymbol(tokens[i])
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

func ReadTokens(tokens []string) Atom {
	list, _ := readTokens(tokens)
	return list
}

func ReadString(s string) Atom {
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
