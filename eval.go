package gommon

import (
	"errors"
	"fmt"
	"os"
)

func CmdPrint(this *DotPair) (*DotPair, error) {
	list, err := this.Eval()
	if err != nil {
		return nil, err
	}
	list.Print(os.Stdout)
	fmt.Println()
	return nil, nil
}

var builtInFunc = map[string]func(*DotPair) (*DotPair, error){}

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
		tmp := new(DotPair)
		last.Cdr = tmp
		last = tmp
	}
}

func init() {
	builtInFunc["print"] = CmdPrint
}
