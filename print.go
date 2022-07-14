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

func cmdPrinX(w *World, this Node, f func(node Node)) (Node, error) {
	cons, ok := this.(*Cons)
	if !ok {
		return nil, ErrTooFewArguments
	}
	if HasValue(cons.Cdr) {
		return nil, ErrTooManyArguments
	}
	value, err := cons.GetCar().Eval(w)
	if err != nil {
		return nil, err
	}
	f(value)
	return value, nil
}

func cmdPrint(w *World, this Node) (Node, error) {
	terpri(w.Stdout)
	return cmdPrinX(w, this, func(node Node) { node.PrintTo(w.Stdout) })
}

func cmdPrin1(w *World, this Node) (Node, error) {
	return cmdPrinX(w, this, func(node Node) { node.PrintTo(w.Stdout) })
}

func cmdPrinc(w *World, this Node) (Node, error) {
	return cmdPrinX(w, this, func(node Node) { node.PrincTo(w.Stdout) })
}

func cmdTerpri(w *World, _ Node) (Node, error) {
	terpri(w.Stdout)
	return Null, nil
}
