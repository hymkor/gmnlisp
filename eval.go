package gommon

import (
	"errors"
	"fmt"
)

var builtInFunc = map[string]func(Node) (Node, error){
	"print":       CmdPrint,
	"quote":       CmdQuote,
	"+":           CmdPlus,
	"-":           CmdMinus,
	"cons":        CmdCons,
	"car":         CmdCar,
	"cdr":         CmdCdr,
	"atom":        CmdAtom,
	"eq":          CmdEq,
	"lambda":      CmdLambda,
	"progn":       CmdProgn,
	"setq":        CmdSetq,
	"defun":       CmdDefun,
	"let":         CmdLet,
	"cond":        CmdCond,
	"return":      CmdReturn,
	"return-from": CmdReturnFrom,
}

type Callable interface {
	Node
	Call(Node) (Node, error)
}

var ErrExpectedFunction = errors.New("expected function")

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
	_name, ok := first.(NodeSymbol)
	if !ok {
		return nil, fmt.Errorf("cons: %w", ErrExpectedFunction)
	}
	name := string(_name)
	fn, ok := builtInFunc[name]
	if ok {
		return fn(this.Cdr)
	}
	val, ok := globals[name]
	if !ok {
		return nil, fmt.Errorf("%s: Not found", name)
	}
	_fn, ok := val.(Callable)
	if !ok {
		return nil, fmt.Errorf("%s: Not Callable Object", name)
	}
	return _fn.Call(this.Cdr)
}
