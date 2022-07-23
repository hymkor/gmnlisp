package gmnlisp

import (
	"fmt"
	"os"
	"sort"
	"strconv"
)

func cmdQuote(_ *World, n Node) (Node, error) {
	var argv [1]Node
	if err := listToArray(n, argv[:]); err != nil {
		return nil, err
	}
	return argv[0], nil
}

func cmdAtom(w *World, n Node) (Node, error) {
	var argv [1]Node
	if err := w.evalListAll(n, argv[:]); err != nil {
		return nil, err
	}
	if _, ok := argv[0].(*Cons); ok {
		return Null, nil
	}
	return True, nil
}

func cmdEqual(w *World, param Node) (Node, error) {
	first, rest, err := w.shiftAndEvalCar(param)
	if err != nil {
		return nil, err
	}
	for HasValue(rest) {
		var next Node

		next, rest, err = w.shiftAndEvalCar(rest)
		if err != nil {
			return nil, err
		}
		if !first.Equals(next, EQUAL) {
			return Null, nil
		}
	}
	return True, nil
}

func cmdParseInt(w *World, node Node) (Node, error) {
	var argv [1]Node

	if err := w.evalListAll(node, argv[:]); err != nil {
		return nil, err
	}

	s, ok := argv[0].(String)
	if !ok {
		return nil, ErrExpectedString
	}
	value, err := strconv.Atoi(string(s))
	if err != nil {
		return Null, nil
	}
	return Integer(value), nil
}

func cmdQuit(*World, Node) (Node, error) {
	return Null, ErrQuit
}

func cmdGetAllSymbols(w *World, n Node) (Node, error) {
	names := []string{}
	var cons Node = Null
	w.each(func(name string, _ Node) bool {
		names = append(names, name)
		return true
	})
	sort.Strings(names)
	for i := len(names) - 1; i >= 0; i-- {
		cons = &Cons{
			Car: String(names[i]),
			Cdr: cons,
		}
	}
	return cons, nil
}

func cmdForeach(w *World, n Node) (Node, error) {
	cons, ok := n.(*Cons)
	if !ok {
		return nil, fmt.Errorf("(1): %w", ErrExpectedCons)
	}
	symbol, ok := cons.Car.(Symbol)
	if !ok {
		return nil, fmt.Errorf("(1): %w", ErrExpectedSymbol)
	}

	list, code, err := w.shiftAndEvalCar(cons.Cdr)
	if err != nil {
		return nil, err
	}

	var last Node
	for HasValue(list) {
		var err error

		cons, ok := list.(*Cons)
		if !ok {
			return nil, ErrExpectedCons
		}
		w.Set(string(symbol), cons.Car)
		list = cons.Cdr

		last, err = progn(w, code)
		if err != nil {
			return nil, err
		}
	}
	return last, nil
}

func cmdListp(w *World, n Node) (Node, error) {
	var argv [1]Node
	if err := w.evalListAll(n, argv[:]); err != nil {
		return nil, err
	}
	if IsNull(argv[0]) {
		return True, nil
	}
	if _, ok := argv[0].(*Cons); ok {
		return True, nil
	}
	return Null, nil
}

func cmdNot(w *World, n Node) (Node, error) {
	var args [1]Node
	if err := w.evalListAll(n, args[:]); err != nil {
		return nil, err
	}
	if IsNull(args[0]) {
		return True, nil
	}
	return Null, nil
}

func cmdLoad(w *World, n Node) (Node, error) {
	var args [1]Node
	if err := w.evalListAll(n, args[:]); err != nil {
		return nil, err
	}
	fname, ok := args[0].(String)
	if !ok {
		return nil, ErrExpectedString
	}
	script, err := os.ReadFile(string(fname))
	if err != nil {
		return nil, err
	}
	return w.InterpretBytes(script)
}

func cmdNotEqual(w *World, n Node) (Node, error) {
	var args [2]Node
	if err := w.evalListAll(n, args[:]); err != nil {
		return nil, err
	}
	if args[0].Equals(args[1], EQUALP) {
		return Null, nil
	}
	return True, nil
}

func cmdRead(w *World, n Node) (Node, error) {
	var args [1]Node
	if err := w.evalListAll(n, args[:]); err != nil {
		return nil, err
	}
	script, ok := args[0].(String)
	if !ok {
		return nil, ErrExpectedString
	}
	nodes, err := ReadString(string(script))
	if err != nil {
		return nil, err
	}
	if len(nodes) < 1 {
		return Null, nil
	}
	return nodes[0], nil
}

func cmdFunction(w *World, node Node) (Node, error) {
	var argv [1]Node
	if err := w.evalListAll(node, argv[:]); err != nil {
		return nil, err
	}
	f, ok := argv[0].(_Callable)
	if !ok {
		return nil, ErrExpectedFunction
	}
	return f, nil
}
