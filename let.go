package gmnlisp

import (
	"fmt"
)

func CmdLet(param Node) (Node, error) {
	cons, ok := param.(*Cons)
	if !ok {
		return nil, fmt.Errorf("%w: `%s`", ErrExpectedCons, Node2String(param))
	}
	code := cons.Cdr

	backups := map[string]Node{}
	nobackups := map[string]struct{}{}

	err := ForEachQuote(cons.Car, func(node Node) error {
		cons, ok := node.(*Cons)
		if !ok {
			return fmt.Errorf("%w: `%s`",
				ErrExpectedCons, Node2String(node))
		}
		_name, ok := cons.Car.(NodeSymbol)
		if !ok {
			return fmt.Errorf("%w: `%s`",
				ErrExpectedSymbol, Node2String(cons.Car))
		}
		name := string(_name)
		cons, ok = cons.Cdr.(*Cons)
		if !ok {
			return fmt.Errorf("%w: `%s`",
				ErrExpectedCons, Node2String(cons.Cdr))
		}
		value, err := cons.Car.Eval()
		if err != nil {
			return err
		}
		if val, ok := globals[name]; ok {
			backups[name] = val
		} else {
			nobackups[name] = struct{}{}
		}
		globals[name] = value
		return nil
	})
	defer func() {
		for name := range nobackups {
			delete(globals, name)
		}
		for name, value := range backups {
			globals[name] = value
		}
	}()

	if err != nil {
		return nil, err
	}

	result, err := progn(code)
	if err != nil {
		return result, err
	}
	return result, nil
}
