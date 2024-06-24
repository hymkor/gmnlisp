package gmnlisp

import (
	"context"
	"os"
)

func cmdQuote(_ context.Context, _ *World, n Node) (Node, error) {
	var argv [1]Node
	if err := ListToArray(n, argv[:]); err != nil {
		return nil, err
	}
	return argv[0], nil
}

func backQuote(ctx context.Context, w *World, n Node) (Node, error) {
	if cons, ok := n.(*Cons); ok {
		if cons.Car == commaSymbol {
			if cdrCons, ok := cons.Cdr.(*Cons); ok {
				newCar, err := cdrCons.Car.Eval(ctx, w)
				if err != nil {
					return nil, err
				}
				newCdr, err := backQuote(ctx, w, cdrCons.Cdr)
				if err != nil {
					return nil, err
				}
				return &Cons{Car: newCar, Cdr: newCdr}, nil
			}
			return cons.Cdr.Eval(ctx, w)
		}
		newCar, err := backQuote(ctx, w, cons.Car)
		if err != nil {
			return nil, err
		}
		newCdr, err := backQuote(ctx, w, cons.Cdr)
		if err != nil {
			return nil, err
		}
		return &Cons{Car: newCar, Cdr: newCdr}, nil
	}
	return n, nil
}

func cmdBackQuote(ctx context.Context, w *World, n Node) (Node, error) {
	value, _, err := Shift(n)
	if err != nil {
		return nil, err
	}
	return backQuote(ctx, w, value)
}

func funAtom(_ context.Context, _ *World, argv []Node) (Node, error) {
	if _, ok := argv[0].(*Cons); ok {
		return Null, nil
	}
	return True, nil
}

func equalSub(ctx context.Context, w *World, list Node, eq func(Node, Node) bool) (Node, error) {
	first, rest, err := w.ShiftAndEvalCar(ctx, list)
	if err != nil {
		return nil, err
	}
	for IsSome(rest) {
		var next Node

		next, rest, err = w.ShiftAndEvalCar(ctx, rest)
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

func funNot(_ context.Context, w *World, argv []Node) (Node, error) {
	if IsNone(argv[0]) {
		return True, nil
	}
	return Null, nil
}

func funLoad(ctx context.Context, w *World, argv []Node) (Node, error) {
	fname, err := ExpectString(argv[0])
	if err != nil {
		return nil, err
	}
	script, err := os.ReadFile(fname.String())
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
	if IsNone(args[0]) {
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

func funParseNumber(_ context.Context, _ *World, args []Node) (Node, error) {
	s, err := ExpectString(args[0])
	if err != nil {
		return nil, err
	}
	val, ok, err := tryParseAsNumber(s.String())
	if !ok {
		return nil, ErrCanNotParseNumber
	}
	return val, err
}
