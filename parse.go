package gommon

import (
	"fmt"
	"io"
	"strings"
)

type Atom interface {
	Dump(io.Writer)
}

type AtomString struct {
	Value1 string
}

func (this *AtomString) Dump(w io.Writer) {
	fmt.Fprintf(w, "\"%s\"", this.Value1)
}

type AtomSymbol struct {
	Name1 string
}

func (this *AtomSymbol) Dump(w io.Writer) {
	fmt.Fprintf(w, "{%s}", this.Name1)
}

type Node struct {
	Car Atom
	Cdr *Node
}

func parseTokens(tokens []string) (*Node, int) {
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
			car, n := parseTokens(tokens[i:])
			last.Car = car
			i += n
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

func ParseTokens(tokens []string) *Node {
	list, _ := parseTokens(tokens)
	return list
}

func ParseString(s string) *Node {
	return ParseTokens(StringToTokens(s))
}

func (this *Node) Dump(w io.Writer) {
	for p := this; p != nil; p = p.Cdr {
		if p != this {
			fmt.Fprint(w, " ")
		}
		if p.Car == nil {
			fmt.Fprint(w, "<nil>")
		} else if val, ok := p.Car.(*Node); ok {
			fmt.Fprint(w, "(")
			if val == nil {
				fmt.Fprint(w, "<nil>")
			} else {
				val.Dump(w)
			}
			fmt.Fprint(w, ")")
		} else {
			p.Car.Dump(w)
		}
	}
}
