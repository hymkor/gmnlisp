package gmnlisp

import (
	"errors"
	"fmt"
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

func cmdReturn(w *World, n Node) (Node, error) {
	var argv [1]Node
	if err := w.evalListAll(n, argv[:]); err != nil {
		return nil, err
	}
	return nil, &ErrEarlyReturns{Value: argv[0], Name: ""}
}

func cmdReturnFrom(w *World, n Node) (Node, error) {
	var argv [2]Node
	if err := listToSlice(n, argv[:]); err != nil {
		return nil, err
	}
	symbol, ok := argv[0].(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}
	value, err := argv[1].Eval(w)
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

func cmdIf(w *World, param Node) (Node, error) {
	var argv [3]Node
	if err := listToSlice(param, argv[:]); err != nil {
		return nil, err
	}
	cond, err := argv[0].Eval(w)
	if err != nil {
		return nil, err
	}
	if HasValue(cond) {
		return argv[1].Eval(w)
	} else {
		return argv[2].Eval(w)
	}
}
