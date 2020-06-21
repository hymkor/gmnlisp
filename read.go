package gommon

import (
	"fmt"
	"io"
	"regexp"
	"strconv"
	"strings"
)

type Atom interface {
	io.WriterTo
}

type AtomString struct {
	Value1 string
}

func (this *AtomString) WriteTo(w io.Writer) (int64, error) {
	n, err := fmt.Fprintf(w, "\"%s\"", this.Value1)
	return int64(n), err
}

type AtomSymbol struct {
	Name1 string
}

func (this *AtomSymbol) WriteTo(w io.Writer) (int64, error) {
	n, err := fmt.Fprintf(w, "{%s}", this.Name1)
	return int64(n), err
}

type AtomInteger struct {
	Value1 int64
}

func (this *AtomInteger) WriteTo(w io.Writer) (int64, error) {
	n, err := fmt.Fprintf(w, "%d", this.Value1)
	return int64(n), err
}

type Node struct {
	Car Atom
	Cdr *Node
}

var RxNumber = regexp.MustCompile("^[0-9]+$")

func readTokens(tokens []string) (*Node, int) {
	if len(tokens) <= 0 {
		return nil, 0
	}
	if tokens[0] == ")" {
		return nil, 1
	}
	first := new(Node)
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
			last.Car = &AtomInteger{val}
			i++
		} else {
			if strings.HasPrefix(tokens[i], "\"") {
				last.Car = &AtomString{strings.Replace(tokens[i], "\"", "", -1)}
			} else {
				last.Car = &AtomSymbol{tokens[i]}
			}
			i++
		}
		if i >= len(tokens) {
			last.Cdr = nil
			return first, i
		}
		if tokens[i] == ")" {
			last.Cdr = nil
			return first, i + 1
		}
		tmp := new(Node)
		last.Cdr = tmp
		last = tmp
	}
}

func ReadTokens(tokens []string) *Node {
	list, _ := readTokens(tokens)
	return list
}

func ReadString(s string) *Node {
	return ReadTokens(StringToTokens(s))
}

func (this *Node) WriteTo(w io.Writer) (int64, error) {
	var n int64 = 0
	for p := this; p != nil; p = p.Cdr {
		if p != this {
			m, err := fmt.Fprint(w, " ")
			n += int64(m)
			if err != nil {
				return n, err
			}
		}
		if p.Car == nil {
			m, err := fmt.Fprint(w, "<nil>")
			n += int64(m)
			if err != nil {
				return n, err
			}
		} else if val, ok := p.Car.(*Node); ok {
			m, err := fmt.Fprint(w, "(")
			n += int64(m)
			if err != nil {
				return n, err
			}
			if val == nil {
				m, err := fmt.Fprint(w, "<nil>")
				n += int64(m)
				if err != nil {
					return n, err
				}
			} else {
				m, err := val.WriteTo(w)
				n += m
				if err != nil {
					return n, err
				}
			}
			m, err = fmt.Fprint(w, ")")
			n += int64(m)
			if err != nil {
				return n, err
			}
		} else {
			m, err := p.Car.WriteTo(w)
			n += int64(m)
			if err != nil {
				return n, err
			}
		}
	}
	return n, nil
}
