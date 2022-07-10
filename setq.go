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
