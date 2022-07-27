package gmnlisp

import (
	"context"
	"fmt"
)

func cmdCar(ctx context.Context, w *World, argv []Node) (Node, error) {
	cons, ok := argv[0].(*Cons)
	if !ok {
		return nil, fmt.Errorf("%w: %s", ErrExpectedCons, toString(argv[0]))
	}
	return cons.Car, nil
}

func cmdCdr(ctx context.Context, w *World, argv []Node) (Node, error) {
	cons, ok := argv[0].(*Cons)
	if !ok {
		return nil, ErrExpectedCons
	}
	return cons.Cdr, nil
}

func nthcdr(n int, list Node) (Node, error) {
	for i := 0; i < n; i++ {
		var err error

		_, list, err = shift(list)
		if err != nil {
			return nil, err
		}
	}
	return list, nil
}

func cmdNthcdr(_ context.Context, _ *World, argv []Node) (Node, error) {
	n, ok := argv[0].(Integer)
	if !ok {
		return nil, ErrExpectedNumber
	}
	return nthcdr(int(n), argv[1])
}

func nth(n int, list Node) (Node, error) {
	var err error
	list, err = nthcdr(n, list)
	if err != nil {
		return nil, err
	}
	cons, ok := list.(*Cons)
	if !ok {
		return Null, nil
	}
	return cons.Car, nil
}

func cmdNth(_ context.Context, _ *World, argv []Node) (Node, error) {
	n, ok := argv[0].(Integer)
	if !ok {
		return nil, ErrExpectedNumber
	}
	return nth(int(n), argv[1])
}

func cmdCadr(ctx context.Context, w *World, argv []Node) (Node, error) {
	return nth(1, argv[0])
}

func cmdCaddr(ctx context.Context, w *World, argv []Node) (Node, error) {
	return nth(2, argv[0])
}

func cmdCadddr(ctx context.Context, w *World, argv []Node) (Node, error) {
	return nth(3, argv[0])
}

func cmdCddr(ctx context.Context, w *World, argv []Node) (Node, error) {
	return nthcdr(2, argv[0])
}

func cmdCdddr(ctx context.Context, w *World, argv []Node) (Node, error) {
	return nthcdr(3, argv[0])
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

func cmdAppend(ctx context.Context, w *World, list Node) (Node, error) {
	first := &Cons{}
	for HasValue(list) {
		var next Node
		var err error

		next, list, err = w.shiftAndEvalCar(ctx, list)
		if err != nil {
			return nil, err
		}
		last, err := lastOfList(first)
		if err != nil {
			return nil, err
		}
		last.Cdr = next
	}
	return first.Cdr, nil
}

func cmdMember(ctx context.Context, w *World, argv []Node) (Node, error) {
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

func cmdCons(ctx context.Context, w *World, argv []Node) (Node, error) {
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

func cmdListp(ctx context.Context, w *World, argv []Node) (Node, error) {
	if IsNull(argv[0]) {
		return True, nil
	}
	if _, ok := argv[0].(*Cons); ok {
		return True, nil
	}
	return Null, nil
}

func cmdLength(ctx context.Context, w *World, argv []Node) (Node, error) {
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

func cmdReverse(ctx context.Context, w *World, argv []Node) (Node, error) {
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

func cmdAssoc(ctx context.Context, w *World, argv []Node) (Node, error) {
	key := argv[0]
	list := argv[1]

	for HasValue(list) {
		var element Node
		var err error

		element, list, err = shift(list)
		if err != nil {
			return nil, err
		}
		cons, ok := element.(*Cons)
		if !ok {
			return nil, ErrExpectedCons
		}
		if key.Equals(cons.Car, EQUAL) {
			return cons, nil
		}
	}
	return Null, nil
}

func subst(newItem, oldItem, list Node) Node {
	if list.Equals(oldItem, EQUAL) {
		return newItem
	}
	cons, ok := list.(*Cons)
	if ok {
		return &Cons{
			Car: subst(newItem, oldItem, cons.Car),
			Cdr: subst(newItem, oldItem, cons.Cdr),
		}
	}
	return list
}

func cmdSubst(ctx context.Context, w *World, argv []Node) (Node, error) {
	return subst(argv[0], argv[1], argv[2]), nil
}
