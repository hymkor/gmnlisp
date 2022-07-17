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

func cmdPrinX(w *World, n Node, f func(node Node)) (Node, error) {
	var argv [1]Node
	if err := w.evalListAll(n, argv[:]); err != nil {
		return nil, err
	}
	f(argv[0])
	return argv[0], nil
}

func cmdPrint(w *World, this Node) (Node, error) {
	terpri(w.Stdout)
	return cmdPrinX(w, this, func(node Node) { node.PrintTo(w.Stdout) })
}

func cmdPrin1(w *World, this Node) (Node, error) {
	return cmdPrinX(w, this, func(node Node) { node.PrintTo(w.Stdout) })
}

func cmdPrinc(w *World, this Node) (Node, error) {
	return cmdPrinX(w, this, func(node Node) { princTo(node, w.Stdout) })
}

func cmdTerpri(w *World, _ Node) (Node, error) {
	terpri(w.Stdout)
	return Null, nil
}
