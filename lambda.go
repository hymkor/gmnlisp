package gmnlisp

import (
	"errors"
	"fmt"
	"io"
)

type ErrEarlyReturns struct {
	Value Node
	Name  string
}

func (e *ErrEarlyReturns) Error() string {
	if e.Name == "" {
		return "Unexpected (return)"
	}
	return fmt.Sprintf("Unexpected (return-from %s)", e.Name)
}

func cmdReturn(ins *Instance, node Node) (Node, error) {
	cons, ok := node.(*Cons)
	if !ok {
		return nil, ErrExpectedCons
	}
	value, err := cons.GetCar().Eval(ins)
	if err != nil {
		return nil, err
	}
	return nil, &ErrEarlyReturns{Value: value, Name: ""}
}

func cmdReturnFrom(ins *Instance, node Node) (Node, error) {
	cons, ok := node.(*Cons)
	if !ok {
		return nil, ErrExpectedCons
	}
	symbol, ok := cons.Car.(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}
	cons, ok = cons.Cdr.(*Cons)
	if !ok {
		return nil, ErrExpectedCons
	}
	value, err := cons.GetCar().Eval(ins)
	if err != nil {
		return nil, err
	}
	return nil, &ErrEarlyReturns{Value: value, Name: string(symbol)}
}

func progn(ins *Instance, c Node) (Node, error) {
	var last Node
	for HasValue(c) {
		cons, ok := c.(*Cons)
		if !ok {
			return nil, ErrExpectedCons
		}
		var err error
		last, err = cons.GetCar().Eval(ins)
		if err != nil {
			return nil, err
		}
		c = cons.Cdr
	}
	return last, nil
}

func cmdProgn(ins *Instance, c Node) (Node, error) {
	return progn(ins, c)
}

type Lambda struct {
	param []string
	code  Node
	name  string
}

func cmdLambda(_ *Instance, node Node) (Node, error) {
	return newLambda(node, "")
}

func newLambda(node Node, blockName string) (Node, error) {
	// (lambda (param) code)

	cons, ok := node.(*Cons)
	if !ok {
		return nil, fmt.Errorf("%w for parameter list", ErrExpectedCons)
	}
	params := []string{}
	if err := forEachWithoutEval(cons.Car, func(n Node) error {
		name, ok := n.(Symbol)
		if !ok {
			return ErrExpectedSymbol
		}
		params = append(params, string(name))
		return nil
	}); err != nil {
		return nil, err
	}

	return &Lambda{
		param: params,
		code:  cons.GetCdr(),
		name:  blockName,
	}, nil
}

func (nl *Lambda) PrintTo(w io.Writer) {
	nl.prinX(w, true)
}

func (nl *Lambda) PrincTo(w io.Writer) {
	nl.prinX(w, false)
}

func (NL *Lambda) prinX(w io.Writer, rich bool) {
	io.WriteString(w, "(lambda (")
	dem := ""
	for _, name := range NL.param {
		io.WriteString(w, dem)
		io.WriteString(w, name)
		dem = " "
	}
	io.WriteString(w, ") ")
	if cons, ok := NL.code.(*Cons); ok {
		cons.writeToWithoutKakko(w, rich)
	} else {
		if rich {
			NL.code.PrintTo(w)
		} else {
			NL.code.PrincTo(w)
		}
	}
	io.WriteString(w, ")")
}

func (*Lambda) IsNull() bool {
	return false
}

func (NL *Lambda) Call(ins *Instance, n Node) (Node, error) {
	backups := map[string]Node{}
	nobackups := map[string]struct{}{}
	for _, name := range NL.param {
		if value, ok := ins.globals[name]; ok {
			backups[name] = value
		} else {
			nobackups[name] = struct{}{}
		}

		if cons, ok := n.(*Cons); ok {
			var err error
			ins.globals[name], err = cons.GetCar().Eval(ins)
			if err != nil {
				return nil, err
			}
			n = cons.GetCdr()
		} else {
			ins.globals[name] = Null
		}
	}
	defer func() {
		for name := range nobackups {
			delete(ins.globals, name)
		}
		for name, value := range backups {
			ins.globals[name] = value
		}
	}()
	var errEarlyReturns *ErrEarlyReturns

	result, err := progn(ins, NL.code)
	if errors.As(err, &errEarlyReturns) && errEarlyReturns.Name == string(NL.name) {
		return errEarlyReturns.Value, nil
	}
	return result, err
}

func (NL *Lambda) Eval(*Instance) (Node, error) {
	return NL, nil
}

func (*Lambda) Equals(Node) bool {
	return false
}

func cmdDefun(ins *Instance, node Node) (Node, error) {
	cons, ok := node.(*Cons)
	if !ok {
		return nil, ErrExpectedCons
	}
	_name, ok := cons.Car.(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}
	name := string(_name)

	lambda, err := newLambda(cons.Cdr, string(_name))
	if err != nil {
		return nil, err
	}
	ins.globals[name] = lambda
	return lambda, nil
}

func cmdBlock(ins *Instance, node Node) (Node, error) {
	cons, ok := node.(*Cons)
	if !ok {
		return nil, ErrExpectedCons
	}
	_name, ok := cons.Car.(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}
	name := string(_name)

	var errEarlyReturns *ErrEarlyReturns
	rv, err := progn(ins, cons.Cdr)
	if errors.As(err, &errEarlyReturns) && errEarlyReturns.Name == name {
		return errEarlyReturns.Value, nil
	}
	return rv, err
}

func cmdCond(ins *Instance, node Node) (Node, error) {
	for HasValue(node) {
		cons, ok := node.(*Cons)
		if !ok {
			return nil, ErrExpectedCons
		}
		node = cons.Cdr

		conditionAndActions, ok := cons.Car.(*Cons)
		if !ok {
			return nil, fmt.Errorf("%w: %s", ErrExpectedCons, toString(cons.Car))
		}
		condition, err := conditionAndActions.GetCar().Eval(ins)
		if err != nil {
			return nil, err
		}
		if HasValue(condition) {
			result, err := progn(ins, conditionAndActions.Cdr)
			if err != nil {
				return result, err
			}
			return result, err
		}
	}
	return Null, nil
}
