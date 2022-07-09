package gommon

import (
	"fmt"
	"os"
)

func cmdPrinX(this Node, f func(node Node)) (Node, error) {
	cons, ok := this.(*Cons)
	if !ok || !IsNull(cons.Cdr) {
		return nil, fmt.Errorf("%w: `%s`", ErrTooFewOrTooManyArguments, this)
	}
	value, err := cons.GetCar().Eval()
	if err != nil {
		return nil, err
	}
	f(value)
	return value, nil
}

func CmdPrint(this Node) (Node, error) {
	fmt.Println()
	return cmdPrinX(this, func(node Node) { node.PrintTo(os.Stdout) })
}

func CmdPrin1(this Node) (Node, error) {
	return cmdPrinX(this, func(node Node) { node.PrintTo(os.Stdout) })
}

func CmdPrinc(this Node) (Node, error) {
	return cmdPrinX(this, func(node Node) { node.PrincTo(os.Stdout) })
}

func CmdTerpri(Node) (Node, error) {
	fmt.Println()
	return NullValue, nil
}
