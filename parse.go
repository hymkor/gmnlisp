package gommon

import (
	"fmt"
	"io"
)

type Node struct {
	Car interface{}
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
			last.Car = tokens[i]
			i++
		}
		if i >= len(tokens) {
			last.Cdr = nil
			return first, i
		}
		if tokens[i] == ")" {
			last.Cdr = nil
			return first, i+1
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

func (this *Node) Print(w io.Writer) {
	for p := this; p != nil; p = p.Cdr {
		if p != this {
			fmt.Fprint(w, " ")
		}
		switch t := p.Car.(type) {
		case nil:
			fmt.Fprint(w, "<nil>")
		case string:
			fmt.Fprintf(w, "\"%s\"", t)
		case *Node:
			if t == nil {
				fmt.Fprint(w, "<nil>")
			} else {
				fmt.Fprint(w, "(")
				t.Print(w)
				fmt.Fprint(w, ")")
			}
		}
	}
}
