package gmnlisp

import (
	"fmt"
)

func cmdSetq(ins *Instance, node Node) (Node, error) {
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
			value, err = n.Eval(ins)
			if err != nil {
				return err
			}
			ins.globals[name] = value
			name = ""
		}
		return nil
	})
	return value, err
}

func cmdLet(ins *Instance, param Node) (Node, error) {
	cons, ok := param.(*Cons)
	if !ok {
		return nil, fmt.Errorf("%w: `%s`", ErrExpectedCons, toString(param))
	}
	code := cons.Cdr

	backups := map[string]Node{}
	nobackups := map[string]struct{}{}

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
		value, err := cons.Car.Eval(ins)
		if err != nil {
			return err
		}
		if val, ok := ins.globals[name]; ok {
			backups[name] = val
		} else {
			nobackups[name] = struct{}{}
		}
		ins.globals[name] = value
		return nil
	})
	defer func() {
		for name := range nobackups {
			delete(ins.globals, name)
		}
		for name, value := range backups {
			ins.globals[name] = value
		}
	}()

	if err != nil {
		return nil, err
	}

	result, err := progn(ins, code)
	if err != nil {
		return result, err
	}
	return result, nil
}
