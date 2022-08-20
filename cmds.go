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

func funAtom(_ context.Context, _ *World, argv []Node) (Node, error) {
	if _, ok := argv[0].(*Cons); ok {
		return Null, nil
	}
	return True, nil
}

func equalSub(ctx context.Context, w *World, list Node, eq func(Node, Node) bool) (Node, error) {
	first, rest, err := w.shiftAndEvalCar(ctx, list)
	if err != nil {
		return nil, err
	}
	for HasValue(rest) {
		var next Node

		next, rest, err = w.shiftAndEvalCar(ctx, rest)
		if err != nil {
			return nil, err
		}
		if !eq(first, next) {
			return Null, nil
		}
	}
	return True, nil

}

func cmdEq(ctx context.Context, w *World, list Node) (Node, error) {
	return equalSub(ctx, w, list, func(left, right Node) bool {
		return left == right
	})
}

func cmdEql(ctx context.Context, w *World, list Node) (Node, error) {
	return equalSub(ctx, w, list, func(left, right Node) bool {
		return left.Equals(right, STRICT)
	})
}

func cmdEqual(ctx context.Context, w *World, list Node) (Node, error) {
	return equalSub(ctx, w, list, func(left, right Node) bool {
		return left.Equals(right, EQUAL)
	})
}

func cmdGetAllSymbols(_ context.Context, w *World, n Node) (Node, error) {
	for w.parent != nil {
		w = w.parent
	}

	names := []string{}
	w.lexical.All(func(name Symbol, _ Node) bool {
		names = append(names, string(name))
		return true
	})
	sort.Strings(names)

	var cons Node = Null
	for i := len(names) - 1; i >= 0; i-- {
		cons = &Cons{
			Car: String(names[i]),
			Cdr: cons,
		}
	}
	return cons, nil
}

func funNot(_ context.Context, w *World, argv []Node) (Node, error) {
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

func funNotEqual(_ context.Context, _ *World, argv []Node) (Node, error) {
	if argv[0].Equals(argv[1], EQUALP) {
		return Null, nil
	}
	return True, nil
}

func funRead(_ context.Context, _ *World, args []Node) (Node, error) {
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

func funZerop(_ context.Context, _ *World, args []Node) (Node, error) {
	return xxxxP(args,
		func(value Integer) bool { return value == 0 },
		func(value Float) bool { return value == 0 })
}

func funNumberp(_ context.Context, _ *World, args []Node) (Node, error) {
	return xxxxP(args,
		func(Integer) bool { return true },
		func(Float) bool { return true })
}

func funPlusp(_ context.Context, _ *World, args []Node) (Node, error) {
	return xxxxP(args,
		func(value Integer) bool { return value > 0 },
		func(value Float) bool { return value > 0 })
}

func funMinusp(_ context.Context, _ *World, args []Node) (Node, error) {
	return xxxxP(args,
		func(value Integer) bool { return value < 0 },
		func(value Float) bool { return value < 0 })
}

func funOddp(_ context.Context, _ *World, args []Node) (Node, error) {
	if value, ok := args[0].(Integer); ok && value%2 == 1 {
		return True, nil
	}
	return Null, nil
}

func funEvenp(_ context.Context, _ *World, args []Node) (Node, error) {
	if value, ok := args[0].(Integer); ok && value%2 == 0 {
		return True, nil
	}
	return Null, nil
}

func funNullp(_ context.Context, _ *World, args []Node) (Node, error) {
	if IsNull(args[0]) {
		return True, nil
	}
	return Null, nil
}

func funAnyTypep[T Node](_ context.Context, _ *World, args []Node) (Node, error) {
	if _, ok := args[0].(T); ok {
		return True, nil
	}
	return Null, nil
}
