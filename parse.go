package gommon

import (
	"fmt"
	"io"
)

type DotPair struct {
	Car interface{}
	Cdr *DotPair
}

func parseTokens(tokens []string) (*DotPair, int) {
	if len(tokens) <= 0 {
		return nil, 0
	}
	if tokens[0] == ")" {
		return nil, 1
	}
	first := new(DotPair)
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
		if i >= len(tokens) || tokens[i] == ")" {
			last.Cdr = nil
			return first, i
		}
		tmp := new(DotPair)
		last.Cdr = tmp
		last = tmp
	}
}

func ParseTokens(tokens []string) *DotPair {
	list, _ := parseTokens(tokens)
	return list
}

func ParseString(s string) *DotPair {
	return ParseTokens(StringToTokens(s))
}

func (this *DotPair) Print(w io.Writer) {
	for p := this; p != nil; p = p.Cdr {
		if p != this {
			fmt.Fprint(w, " ")
		}
		switch t := p.Car.(type) {
		case nil:
			fmt.Fprint(w, "<nil>")
		case string:
			fmt.Fprintf(w, "\"%s\"", t)
		case *DotPair:
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
