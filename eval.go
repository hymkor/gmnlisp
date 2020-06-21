package gommon

import (
	"errors"
	"fmt"
)

var builtInFunc map[string]func(*Node) (*Node, error)

func (this *Node) Eval() (*Node, error) {
	first := new(Node)
	last := first
	p := this
	for {
		if t, ok := p.Car.(*Node); ok {
			if name, ok := t.Car.(*AtomSymbol); ok {
				if fn, ok := builtInFunc[name.Name1]; ok {
					var err error
					last.Car, err = fn(t.Cdr)
					if err != nil {
						return nil, err
					}
				} else {
					return nil, fmt.Errorf("%s: Not found", name)
				}
			} else {
				return nil, errors.New("list: can not evaluate")
			}
		} else {
			last.Car = p.Car
		}
		if p.Cdr == nil {
			last.Cdr = nil
			return first, nil
		}
		p = p.Cdr
		tmp := new(Node)
		last.Cdr = tmp
		last = tmp
	}
}

func init() {
	builtInFunc = map[string]func(*Node) (*Node, error){
		"print": CmdPrint,
		"quote": CmdQuote,
	}
}
