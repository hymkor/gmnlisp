package gommon

import (
	"fmt"
)

func CmdSetq(node Node) (Node, error) {
	var name string
	var value Node = NullValue
	err := ForEachQuote(node, func(n Node) error {
		if name == "" {
			_name, ok := n.(NodeSymbol)
			if !ok {
				return fmt.Errorf("invalid name: %s", Node2String(node))
			}
			name = string(_name)
		} else {
			var err error
			value, err = n.Eval()
			if err != nil {
				return err
			}
			globals[name] = value
			name = ""
		}
		return nil
	})
	return value, err
}
