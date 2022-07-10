package gmnlisp

import (
	"fmt"
)

func cmdSetq(instance *Instance, node Node) (Node, error) {
	var name string
	var value Node = NullValue
	err := forEachWithoutEval(node, func(n Node) error {
		if name == "" {
			_name, ok := n.(NodeSymbol)
			if !ok {
				return fmt.Errorf("%w: `%s`", ErrExpectedSymbol, toString(node))
			}
			name = string(_name)
		} else {
			var err error
			value, err = n.Eval(instance)
			if err != nil {
				return err
			}
			instance.globals[name] = value
			name = ""
		}
		return nil
	})
	return value, err
}
