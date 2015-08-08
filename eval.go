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
		switch t := p.Car.(type) {
		case string:
			last.Car = t
		case nil:
			last.Car = t
		case *Node:
			if name, ok := t.Car.(string); ok {
				if fn, ok := builtInFunc[name]; ok {
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
