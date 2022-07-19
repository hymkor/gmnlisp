package gmnlisp

import (
	"fmt"
	"io"
)

func cmdTerpri(w *World, _ Node) (Node, error) {
	out, err := w.Stdout()
	if err != nil {
		return nil, err
	}
	fmt.Fprintln(out)
	return Null, nil
}

func cmdPrinX(w *World, n Node, f func(node Node, out io.Writer)) (Node, error) {
	var argv [1]Node
	if err := w.evalListAll(n, argv[:]); err != nil {
		return nil, err
	}
	out, err := w.Stdout()
	if err != nil {
		return nil, err
	}
	f(argv[0], out)
	return argv[0], nil
}

func cmdPrint(w *World, this Node) (Node, error) {
	if _, err := cmdTerpri(w, this); err != nil {
		return nil, err
	}
	return cmdPrinX(w, this, func(node Node, out io.Writer) {
		node.PrintTo(out, PRINT)
	})
}

func cmdPrin1(w *World, this Node) (Node, error) {
	return cmdPrinX(w, this, func(node Node, out io.Writer) {
		node.PrintTo(out, PRINT)
	})
}

func cmdPrinc(w *World, this Node) (Node, error) {
	return cmdPrinX(w, this, func(node Node, out io.Writer) {
		node.PrintTo(out, PRINC)
	})
}
