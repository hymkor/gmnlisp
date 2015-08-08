package gommon

import (
	"errors"
	"fmt"
	"os"
)

func CmdPrint(first *DotPair) *DotPair {
	defer fmt.Println()
	for p := first; p != nil; p = p.Cdr {
		if p != first {
			fmt.Print(" ")
		}
		switch t := p.Car.(type) {
		case string:
			fmt.Print(t)
		case *DotPair:
			if p.Car != nil {
				result, err := t.Eval()
				if err != nil {
					return nil
				}
				result.Print(os.Stdout)
			} else {
				fmt.Print("<nil>")
			}
		}
	}
	return nil
}

var builtInFunc = map[string]func(*DotPair) *DotPair{}

func (this *DotPair) Eval() (*DotPair, error) {
	first := new(DotPair)
	last := first
	p := this
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
	builtInFunc["print"] = CmdPrint
}
