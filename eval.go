package gommon

import (
	"errors"
	"fmt"
)

var builtInFunc map[string]func(*Cons) (*Cons, error)

func (this *Cons) Eval() (*Cons, error) {
	first := new(Cons)
	last := first
	p := this
	for {
		if t, ok := p.Car.(*Cons); ok {
			if name, ok := t.Car.(AtomSymbol); ok {
				if fn, ok := builtInFunc[string(name)]; ok {
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
		tmp := new(Cons)
		last.Cdr = tmp
		last = tmp
	}
}

func init() {
	builtInFunc = map[string]func(*Cons) (*Cons, error){
		"print": CmdPrint,
		"quote": CmdQuote,
	}
}
