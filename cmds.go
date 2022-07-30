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

func funAtom(ctx context.Context, w *World, argv []Node) (Node, error) {
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
	w.each(func(name Symbol, _ Node) bool {
		names = append(names, string(name))
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

func funNot(ctx context.Context, w *World, argv []Node) (Node, error) {
	if IsNull(argv[0]) {
		return True, nil
	}
	return Null, nil
}

func funLoad(ctx context.Context, w *World, argv []Node) (Node, error) {
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

func funNotEqual(ctx context.Context, w *World, argv []Node) (Node, error) {
	if argv[0].Equals(argv[1], EQUALP) {
		return Null, nil
	}
	return True, nil
}

func funRead(ctx context.Context, w *World, args []Node) (Node, error) {
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

func xxxxP(args []Node, f1 func(Integer) bool, f2 func(Float) bool) (Node, error) {
	if value, ok := args[0].(Integer); ok {
		if f1(value) {
			return True, nil
		}
	} else if value, ok := args[0].(Float); ok {
		if f2(value) {
			return True, nil
		}
	}
	return Null, nil
}

func funZerop(ctx context.Context, w *World, args []Node) (Node, error) {
	return xxxxP(args,
		func(value Integer) bool { return value == 0 },
		func(value Float) bool { return value == 0 })
}

func funNumberp(ctx context.Context, w *World, args []Node) (Node, error) {
	return xxxxP(args,
		func(Integer) bool { return true },
		func(Float) bool { return true })
}

func funPlusp(ctx context.Context, w *World, args []Node) (Node, error) {
	return xxxxP(args,
		func(value Integer) bool { return value > 0 },
		func(value Float) bool { return value > 0 })
}

func funMinusp(ctx context.Context, w *World, args []Node) (Node, error) {
	return xxxxP(args,
		func(value Integer) bool { return value < 0 },
		func(value Float) bool { return value < 0 })
}

func funOddp(ctx context.Context, w *World, args []Node) (Node, error) {
	if value, ok := args[0].(Integer); ok && value%2 == 1 {
		return True, nil
	}
	return Null, nil
}

func funEvenp(ctx context.Context, w *World, args []Node) (Node, error) {
	if value, ok := args[0].(Integer); ok && value%2 == 0 {
		return True, nil
	}
	return Null, nil
}

func funNullp(ctx context.Context, w *World, args []Node) (Node, error) {
	if IsNull(args[0]) {
		return True, nil
	}
	return Null, nil
}

func funAnyTypep[T Node](ctx context.Context, w *World, args []Node) (Node, error) {
	if _, ok := args[0].(T); ok {
		return True, nil
	}
	return Null, nil
}
