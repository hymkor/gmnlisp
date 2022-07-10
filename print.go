package gmnlisp

import (
	"fmt"
	"os"
)

var terpri func()

func init() {
	terpri = func() { terpri = func() { fmt.Println() } }
}

func cmdPrinX(instance *Instance, this Node, f func(node Node)) (Node, error) {
	cons, ok := this.(*Cons)
	if !ok || HasValue(cons.Cdr) {
		return nil, ErrTooFewOrTooManyArguments
	}
	value, err := cons.GetCar().Eval(instance)
	if err != nil {
		return nil, err
	}
	f(value)
	return value, nil
}

func cmdPrint(instance *Instance, this Node) (Node, error) {
	terpri()
	return cmdPrinX(instance, this, func(node Node) { node.PrintTo(os.Stdout) })
}

func cmdPrin1(instance *Instance, this Node) (Node, error) {
	return cmdPrinX(instance, this, func(node Node) { node.PrintTo(os.Stdout) })
}

func cmdPrinc(instance *Instance, this Node) (Node, error) {
	return cmdPrinX(instance, this, func(node Node) { node.PrincTo(os.Stdout) })
}

func cmdTerpri(*Instance, Node) (Node, error) {
	terpri()
	return NullValue, nil
}
