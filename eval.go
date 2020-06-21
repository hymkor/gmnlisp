package gommon

import (
	"errors"
	"fmt"
)

var builtInFunc map[string]func(Atom) (Atom, error)

func (this *Cons) Eval() (Atom, error) {
	first := this.Car
	if p, ok := first.(*Cons); ok {
		var err error
		first, err = p.Eval()
		if err != nil {
			return nil, err
		}
	}
	name, ok := first.(AtomSymbol)
	if !ok {
		return nil, errors.New("Illeagal function Call")
	}
	fn, ok := builtInFunc[string(name)]
	if !ok {
		return nil, fmt.Errorf("%s: Not found", name)
	}
	return fn(this.Cdr)
}

func init() {
	builtInFunc = map[string]func(Atom) (Atom, error){
		"print": CmdPrint,
		"quote": CmdQuote,
	}
}
