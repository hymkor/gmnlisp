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
	list, err := shiftList(args[0], n)
	if err != nil {
		return nil, err
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

func cmdMapCar(ctx context.Context, w *World, n Node) (Node, error) {
	first, n, err := w.shiftAndEvalCar(ctx, n)
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
	list, err := w.evalListToSlice(ctx, n)
	if err != nil {
		return nil, err
	}
	resultSet := []Node{}
	for {
		paramSet := make([]Node, len(list))
		for i := 0; i < len(list); i++ {
			if IsNull(list[i]) {
				return List(resultSet...), nil
			}
			cons, ok := list[i].(*Cons)
			if !ok {
				return nil, ErrExpectedCons
			}
			paramSet[i] = cons.Car
			list[i] = cons.Cdr
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
