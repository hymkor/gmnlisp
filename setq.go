package gmnlisp

import (
	"fmt"
)

func cmdSetq(w *World, node Node) (Node, error) {
	var name string
	var value Node = Null
	err := forEachList(node, func(n Node) error {
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
	if name != "" {
		return value, ErrTooFewArguments
	}
	return value, err
}

func cmdLet(w *World, param Node) (Node, error) {
	cons, ok := param.(*Cons)
	if !ok {
		return nil, fmt.Errorf("%w: `%s`", ErrExpectedCons, toString(param))
	}
	code := cons.Cdr

	globals := map[string]Node{}

	err := forEachList(cons.Car, func(node Node) error {
		var argv [2]Node

		if err := listToArray(node, argv[:]); err != nil {
			return err
		}
		symbol, ok := argv[0].(Symbol)
		if !ok {
			return fmt.Errorf("%w `%s`", ErrExpectedSymbol, toString(argv[0]))
		}
		value, err := argv[1].Eval(w)
		if err != nil {
			return err
		}
		globals[string(symbol)] = value
		return nil
	})
	if err != nil {
		return nil, err
	}

	w.scope = &_Scope{
		globals: globals,
		parent:  w.scope,
	}
	defer func() {
		w.scope = w.scope.parent
	}()

	return progn(w, code)
}
