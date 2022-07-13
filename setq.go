package gmnlisp

import (
	"fmt"
)

func cmdSetq(w *World, node Node) (Node, error) {
	var name string
	var value Node = Null
	err := forEachWithoutEval(node, func(n Node) error {
		if name == "" {
			_name, ok := n.(Symbol)
			if !ok {
				return fmt.Errorf("%w: `%s`", ErrExpectedSymbol, toString(node))
			}
			name = string(_name)
		} else {
			var err error
			value, err = n.Eval(w)
			if err != nil {
				return err
			}
			w.Set(name, value)
			name = ""
		}
		return nil
	})
	return value, err
}

func cmdLet(w *World, param Node) (Node, error) {
	cons, ok := param.(*Cons)
	if !ok {
		return nil, fmt.Errorf("%w: `%s`", ErrExpectedCons, toString(param))
	}
	code := cons.Cdr

	globals := map[string]Node{}

	err := forEachWithoutEval(cons.Car, func(node Node) error {
		cons, ok := node.(*Cons)
		if !ok {
			return fmt.Errorf("%w: `%s`",
				ErrExpectedCons, toString(node))
		}
		_name, ok := cons.Car.(Symbol)
		if !ok {
			return fmt.Errorf("%w: `%s`",
				ErrExpectedSymbol, toString(cons.Car))
		}
		name := string(_name)
		cons, ok = cons.Cdr.(*Cons)
		if !ok {
			return fmt.Errorf("%w: `%s`",
				ErrExpectedCons, toString(cons.Cdr))
		}
		value, err := cons.Car.Eval(w)
		if err != nil {
			return err
		}
		globals[name] = value
		return nil
	})
	w.scope = &_Scope{
		globals: globals,
		parent:  w.scope,
	}
	defer func() {
		w.scope = w.scope.parent
	}()

	if err != nil {
		return nil, err
	}

	result, err := progn(w, code)
	if err != nil {
		return result, err
	}
	return result, nil
}
