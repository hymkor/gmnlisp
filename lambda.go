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

func cmdReturn(w *World, node Node) (Node, error) {
	cons, ok := node.(*Cons)
	if !ok {
		return nil, ErrExpectedCons
	}
	value, err := cons.GetCar().Eval(w)
	if err != nil {
		return nil, err
	}
	return nil, &ErrEarlyReturns{Value: value, Name: ""}
}

func cmdReturnFrom(w *World, node Node) (Node, error) {
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
	value, err := cons.GetCar().Eval(w)
	if err != nil {
		return nil, err
	}
	return nil, &ErrEarlyReturns{Value: value, Name: string(symbol)}
}

func progn(w *World, c Node) (Node, error) {
	var last Node
	for HasValue(c) {
		cons, ok := c.(*Cons)
		if !ok {
			return nil, ErrExpectedCons
		}
		var err error
		last, err = cons.GetCar().Eval(w)
		if err != nil {
			return nil, err
		}
		c = cons.Cdr
	}
	return last, nil
}

func cmdProgn(w *World, c Node) (Node, error) {
	return progn(w, c)
}

type Lambda struct {
	param []string
	code  Node
	name  string
}

func cmdLambda(_ *World, node Node) (Node, error) {
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

func (nl *Lambda) prinX(w io.Writer, rich bool) {
	io.WriteString(w, "(lambda (")
	dem := ""
	for _, name := range nl.param {
		io.WriteString(w, dem)
		io.WriteString(w, name)
		dem = " "
	}
	io.WriteString(w, ") ")
	if cons, ok := nl.code.(*Cons); ok {
		cons.writeToWithoutKakko(w, rich)
	} else {
		if rich {
			nl.code.PrintTo(w)
		} else {
			nl.code.PrincTo(w)
		}
	}
	io.WriteString(w, ")")
}

func (nl *Lambda) Call(w *World, n Node) (Node, error) {
	backups := map[string]Node{}
	nobackups := map[string]struct{}{}
	for _, name := range nl.param {
		if value, ok := w.globals[name]; ok {
			backups[name] = value
		} else {
			nobackups[name] = struct{}{}
		}

		if cons, ok := n.(*Cons); ok {
			var err error
			w.globals[name], err = cons.GetCar().Eval(w)
			if err != nil {
				return nil, err
			}
			n = cons.GetCdr()
		} else {
			w.globals[name] = Null
		}
	}
	defer func() {
		for name := range nobackups {
			delete(w.globals, name)
		}
		for name, value := range backups {
			w.globals[name] = value
		}
	}()
	var errEarlyReturns *ErrEarlyReturns

	result, err := progn(w, nl.code)
	if errors.As(err, &errEarlyReturns) && errEarlyReturns.Name == string(nl.name) {
		return errEarlyReturns.Value, nil
	}
	return result, err
}

func (nl *Lambda) Eval(*World) (Node, error) {
	return nl, nil
}

func (*Lambda) Equals(Node) bool {
	return false
}

func (*Lambda) EqualP(Node) bool {
	return false
}

func cmdDefun(w *World, node Node) (Node, error) {
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
	w.globals[name] = lambda
	return lambda, nil
}

func cmdBlock(w *World, node Node) (Node, error) {
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
	rv, err := progn(w, cons.Cdr)
	if errors.As(err, &errEarlyReturns) && errEarlyReturns.Name == name {
		return errEarlyReturns.Value, nil
	}
	return rv, err
}

func cmdCond(w *World, node Node) (Node, error) {
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
		condition, err := conditionAndActions.GetCar().Eval(w)
		if err != nil {
			return nil, err
		}
		if HasValue(condition) {
			result, err := progn(w, conditionAndActions.Cdr)
			if err != nil {
				return result, err
			}
			return result, err
		}
	}
	return Null, nil
}
