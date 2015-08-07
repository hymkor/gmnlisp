package gommon

import (
	"errors"
	"fmt"
	"os"
)

func print_(p *DotPair) *DotPair {
	defer fmt.Println()
	i := 0
	for p != nil {
		if i > 0 {
			fmt.Print(" ")
		}
		i++
		switch t := p.Car.(type) {
		case string:
			fmt.Print(t)
		case *DotPair:
			if p.Car != nil {
				result, err := Eval(t)
				if err != nil {
					return nil
				}
				Print(result, os.Stdout)
			} else {
				fmt.Print("<nil>")
			}
		}
		p = p.Cdr
	}
	return nil
}

var builtInFunc = map[string]func(*DotPair) *DotPair{}

func Eval(p *DotPair) (*DotPair, error) {
	first := new(DotPair)
	last := first
	for {
		switch t := p.Car.(type) {
		case string:
			last.Car = t
		case nil:
			last.Car = t
		case *DotPair:
			if name, ok := t.Car.(string); ok {
				if fn, ok := builtInFunc[name]; ok {
					last.Car = fn(t.Cdr)
				} else {
					return nil, fmt.Errorf("%s: Not found", name)
				}
			} else {
				return nil, errors.New("list: can not evaluate")
			}
		}
		if p.Cdr == nil {
			last.Cdr = nil
			return first, nil
		}
		p = p.Cdr
		tmp := new(DotPair)
		last.Cdr = tmp
		last = tmp
	}
}

func init() {
	builtInFunc["print"] = print_
}
