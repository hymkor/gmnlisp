package gommon

import (
	"errors"
	"fmt"
)

var builtInFunc map[string]func(Node) (Node, error)

type Callable interface {
	Node
	Call(Node) (Node, error)
}

func (this *Cons) Eval() (Node, error) {
	first := this.Car
	if p, ok := first.(*Cons); ok {
		var err error
		first, err = p.Eval()
		if err != nil {
			return nil, err
		}
	}
	if f, ok := first.(Callable); ok {
		return f.Call(this.Cdr)
	}
	name, ok := first.(NodeSymbol)
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
	builtInFunc = map[string]func(Node) (Node, error){
		"print":  CmdPrint,
		"quote":  CmdQuote,
		"+":      CmdPlus,
		"cons":   CmdCons,
		"car":    CmdCar,
		"cdr":    CmdCdr,
		"atom":   CmdAtom,
		"eq":     CmdEq,
		"lambda": CmdLambda,
		"progn":  CmdProgn,
		"setq":   CmdSetq,
	}
}
