package gommon

import (
	"errors"
	"fmt"
)

func CmdLet(param Node) (Node, error) {
	cons, ok := param.(*Cons)
	if !ok {
		return nil, errors.New("let: not a list")
	}
	code := cons.Cdr

	backups := map[string]Node{}
	nobackups := map[string]struct{}{}

	err := ForEachQuote(cons.Car, func(node Node) error {
		cons, ok := node.(*Cons)
		if !ok {
			return fmt.Errorf("let: var list is not a list")
		}
		_name, ok := cons.Car.(NodeSymbol)
		if !ok {
			return fmt.Errorf("let: var name is invalid")
		}
		name := string(_name)
		cons, ok = cons.Cdr.(*Cons)
		if !ok {
			return fmt.Errorf("let: var value is invalid")
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
	return CmdProgn(code)
}
