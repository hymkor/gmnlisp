package gmnlisp

import (
	"context"
	"fmt"
)

func cmdCar(ctx context.Context, w *World, n Node) (Node, error) {
	var argv [1]Node
	if err := w.evalListAll(ctx, n, argv[:]); err != nil {
		return nil, err
	}
	cons, ok := argv[0].(*Cons)
	if !ok {
		return nil, fmt.Errorf("%w: %s", ErrExpectedCons, toString(argv[0]))
	}
	return cons.Car, nil
}

func cmdCdr(ctx context.Context, w *World, n Node) (Node, error) {
	var argv [1]Node
	if err := w.evalListAll(ctx, n, argv[:]); err != nil {
		return nil, err
	}
	cons, ok := argv[0].(*Cons)
	if !ok {
		return nil, ErrExpectedCons
	}
	return cons.Cdr, nil
}

func nthcdr(ctx context.Context, w *World, node Node, n int) (Node, error) {
	var args [1]Node
	if err := w.evalListAll(ctx, node, args[:]); err != nil {
		return nil, err
	}
	list := args[0]
	for i := 0; i < n; i++ {
		var err error

		_, list, err = shift(list)
		if err != nil {
			return nil, err
		}
	}
	return list, nil
}

func nth(ctx context.Context, w *World, node Node, n int) (Node, error) {
	list, err := nthcdr(ctx, w, node, n)
	if err != nil {
		return nil, err
	}
	cons, ok := list.(*Cons)
	if !ok {
		return Null, nil
	}
	return cons.Car, nil
}

func cmdCadr(ctx context.Context, w *World, n Node) (Node, error) {
	return nth(ctx, w, n, 1)
}

func cmdCaddr(ctx context.Context, w *World, n Node) (Node, error) {
	return nth(ctx, w, n, 2)
}

func cmdCadddr(ctx context.Context, w *World, n Node) (Node, error) {
	return nth(ctx, w, n, 3)
}

func cmdCddr(ctx context.Context, w *World, n Node) (Node, error) {
	return nthcdr(ctx, w, n, 2)
}

func cmdCdddr(ctx context.Context, w *World, n Node) (Node, error) {
	return nthcdr(ctx, w, n, 3)
}

func cmdList(ctx context.Context, w *World, node Node) (Node, error) {
	car, rest, err := w.shiftAndEvalCar(ctx, node)
	if err != nil {
		return nil, err
	}
	var cdr Node

	if IsNull(rest) {
		cdr = Null
	} else {
		cdr, err = cmdList(ctx, w, rest)
		if err != nil {
			return nil, err
		}
	}
	return &Cons{Car: car, Cdr: cdr}, nil
}

func lastOfList(node Node) (*Cons, error) {
	for {
		cons, ok := node.(*Cons)
		if !ok {
			return nil, fmt.Errorf("%w `%s`", ErrExpectedCons, node)
		}
		if IsNull(cons.Cdr) {
			return cons, nil
		}
		node = cons.Cdr
	}
}

func cmdAppend(ctx context.Context, w *World, node Node) (Node, error) {
	first, rest, err := w.shiftAndEvalCar(ctx, node)
	if err != nil {
		return nil, err
	}
	for HasValue(rest) {
		var next Node

		next, rest, err = w.shiftAndEvalCar(ctx, rest)
		if err != nil {
			return nil, err
		}
		last, err := lastOfList(first)
		if err != nil {
			return nil, err
		}
		last.Cdr = next
	}
	return first, nil
}

func cmdMember(ctx context.Context, w *World, n Node) (Node, error) {
	var argv [2]Node
	if err := w.evalListAll(ctx, n, argv[:]); err != nil {
		return nil, err
	}
	expr := argv[0]
	list := argv[1]
	for HasValue(list) {
		cons, ok := list.(*Cons)
		if !ok {
			return nil, ErrExpectedCons
		}
		if expr.Equals(cons.Car, EQUAL) {
			return list, nil
		}
		list = cons.Cdr
	}
	return Null, nil
}

func cmdCons(ctx context.Context, w *World, n Node) (Node, error) {
	var argv [2]Node
	if err := w.evalListAll(ctx, n, argv[:]); err != nil {
		return nil, err
	}
	return &Cons{Car: argv[0], Cdr: argv[1]}, nil
}

func cmdMapCar(ctx context.Context, w *World, params Node) (Node, error) {
	first, params, err := w.shiftAndEvalCar(ctx, params)
	if err != nil {
		return nil, err
	}
	f, err := first.Eval(ctx, w)
	if err != nil {
		return nil, err
	}
	_f, ok := f.(_Callable)
	if !ok {
		return nil, ErrExpectedFunction
	}

	listSet := []Node{}
	for HasValue(params) {
		var value Node

		value, params, err = w.shiftAndEvalCar(ctx, params)
		if err != nil {
			return nil, err
		}
		listSet = append(listSet, value)
	}

	resultSet := []Node{}
	for {
		paramSet := make([]Node, len(listSet))
		for i := 0; i < len(listSet); i++ {
			if IsNull(listSet[i]) {
				return List(resultSet...), nil
			}
			paramSet[i], listSet[i], err = shift(listSet[i])
			if err != nil {
				return nil, err
			}
		}
		result, err := _f.Call(ctx, w, List(paramSet...))
		if err != nil {
			return nil, err
		}
		resultSet = append(resultSet, result)
	}
}

func cmdListp(ctx context.Context, w *World, n Node) (Node, error) {
	var argv [1]Node
	if err := w.evalListAll(ctx, n, argv[:]); err != nil {
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

func cmdLength(ctx context.Context, w *World, n Node) (Node, error) {
	var argv [1]Node
	if err := w.evalListAll(ctx, n, argv[:]); err != nil {
		return nil, err
	}
	length := 0
	list := argv[0]
	for HasValue(list) {
		cons, ok := list.(*Cons)
		if !ok {
			return nil, ErrExpectedCons
		}
		length++
		list = cons.Cdr
	}
	return Integer(length), nil
}

func cmdReverse(ctx context.Context, w *World, n Node) (Node, error) {
	var argv [1]Node
	if err := w.evalListAll(ctx, n, argv[:]); err != nil {
		return nil, err
	}
	source := argv[0]
	var result Node

	for HasValue(source) {
		var value Node
		var err error

		value, source, err = shift(source)
		if err != nil {
			return nil, err
		}
		result = &Cons{
			Car: value,
			Cdr: result,
		}
	}
	return result, nil
}
