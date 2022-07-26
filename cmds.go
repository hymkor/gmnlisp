package gmnlisp

import (
	"context"
	"os"
	"sort"
)

func cmdQuote(_ context.Context, _ *World, n Node) (Node, error) {
	var argv [1]Node
	if err := listToArray(n, argv[:]); err != nil {
		return nil, err
	}
	return argv[0], nil
}

func cmdAtom(ctx context.Context, w *World, argv []Node) (Node, error) {
	if _, ok := argv[0].(*Cons); ok {
		return Null, nil
	}
	return True, nil
}

func cmdEqual(ctx context.Context, w *World, param Node) (Node, error) {
	first, rest, err := w.shiftAndEvalCar(ctx, param)
	if err != nil {
		return nil, err
	}
	for HasValue(rest) {
		var next Node

		next, rest, err = w.shiftAndEvalCar(ctx, rest)
		if err != nil {
			return nil, err
		}
		if !first.Equals(next, EQUAL) {
			return Null, nil
		}
	}
	return True, nil
}

func cmdGetAllSymbols(ctx context.Context, w *World, n Node) (Node, error) {
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

func cmdNot(ctx context.Context, w *World, argv []Node) (Node, error) {
	if IsNull(argv[0]) {
		return True, nil
	}
	return Null, nil
}

func cmdLoad(ctx context.Context, w *World, argv []Node) (Node, error) {
	fname, ok := argv[0].(String)
	if !ok {
		return nil, ErrExpectedString
	}
	script, err := os.ReadFile(string(fname))
	if err != nil {
		return nil, err
	}
	return w.InterpretBytes(ctx, script)
}

func cmdNotEqual(ctx context.Context, w *World, argv []Node) (Node, error) {
	if argv[0].Equals(argv[1], EQUALP) {
		return Null, nil
	}
	return True, nil
}

func cmdRead(ctx context.Context, w *World, args []Node) (Node, error) {
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
