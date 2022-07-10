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

func cmdReturn(instance *Instance, node Node) (Node, error) {
	cons, ok := node.(*Cons)
	if !ok {
		return nil, ErrExpectedCons
	}
	value, err := cons.GetCar().Eval(instance)
	if err != nil {
		return nil, err
	}
	return nil, &ErrEarlyReturns{Value: value, Name: ""}
}

func cmdReturnFrom(instance *Instance, node Node) (Node, error) {
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
	value, err := cons.GetCar().Eval(instance)
	if err != nil {
		return nil, err
	}
	return nil, &ErrEarlyReturns{Value: value, Name: string(symbol)}
}

func progn(instance *Instance, c Node) (Node, error) {
	var last Node
	for HasValue(c) {
		cons, ok := c.(*Cons)
		if !ok {
			return nil, ErrExpectedCons
		}
		var err error
		last, err = cons.GetCar().Eval(instance)
		if err != nil {
			return nil, err
		}
		c = cons.Cdr
	}
	return last, nil
}

func cmdProgn(instance *Instance, c Node) (Node, error) {
	return progn(instance, c)
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

func (*Lambda) Null() bool {
	return false
}

func (NL *Lambda) Call(instance *Instance, n Node) (Node, error) {
	backups := map[string]Node{}
	nobackups := map[string]struct{}{}
	for _, name := range NL.param {
		if value, ok := instance.globals[name]; ok {
			backups[name] = value
		} else {
			nobackups[name] = struct{}{}
		}

		if cons, ok := n.(*Cons); ok {
			var err error
			instance.globals[name], err = cons.GetCar().Eval(instance)
			if err != nil {
				return nil, err
			}
			n = cons.GetCdr()
		} else {
			instance.globals[name] = NullValue
		}
	}
	defer func() {
		for name := range nobackups {
			delete(instance.globals, name)
		}
		for name, value := range backups {
			instance.globals[name] = value
		}
	}()
	var errEarlyReturns *ErrEarlyReturns

	result, err := progn(instance, NL.code)
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

func cmdDefun(instance *Instance, node Node) (Node, error) {
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
	instance.globals[name] = lambda
	return lambda, nil
}

func cmdBlock(instance *Instance, node Node) (Node, error) {
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
	rv, err := progn(instance, cons.Cdr)
	if errors.As(err, &errEarlyReturns) && errEarlyReturns.Name == name {
		return errEarlyReturns.Value, nil
	}
	return rv, err
}
