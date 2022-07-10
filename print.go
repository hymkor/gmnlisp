package gmnlisp

import (
	"fmt"
	"io"
)

var terpri func(w io.Writer)

func init() {
	terpri = func(w io.Writer) {
		terpri = func(w io.Writer) { fmt.Fprintln(w) }
	}
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

func cmdPrint(ins *Instance, this Node) (Node, error) {
	terpri(ins.Stdout)
	return cmdPrinX(ins, this, func(node Node) { node.PrintTo(ins.Stdout) })
}

func cmdPrin1(ins *Instance, this Node) (Node, error) {
	return cmdPrinX(ins, this, func(node Node) { node.PrintTo(ins.Stdout) })
}

func cmdPrinc(ins *Instance, this Node) (Node, error) {
	return cmdPrinX(ins, this, func(node Node) { node.PrincTo(ins.Stdout) })
}

func cmdTerpri(ins *Instance, _ Node) (Node, error) {
	terpri(ins.Stdout)
	return NullValue, nil
}
