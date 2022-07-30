package gmnlisp

import (
	"context"
	"fmt"
)

func funCar(ctx context.Context, w *World, argv []Node) (Node, error) {
	cons, ok := argv[0].(*Cons)
	if !ok {
		return nil, fmt.Errorf("%w: %s", ErrExpectedCons, toString(argv[0]))
	}
	return cons.Car, nil
}

func funCdr(ctx context.Context, w *World, argv []Node) (Node, error) {
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

func funNthcdr(_ context.Context, _ *World, argv []Node) (Node, error) {
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

func funNth(_ context.Context, _ *World, argv []Node) (Node, error) {
	n, ok := argv[0].(Integer)
	if !ok {
		return nil, ErrExpectedNumber
	}
	return nth(int(n), argv[1])
}

func funCadr(ctx context.Context, w *World, argv []Node) (Node, error) {
	return nth(1, argv[0])
}

func funCaddr(ctx context.Context, w *World, argv []Node) (Node, error) {
	return nth(2, argv[0])
}

func funCadddr(ctx context.Context, w *World, argv []Node) (Node, error) {
	return nth(3, argv[0])
}

func funCddr(ctx context.Context, w *World, argv []Node) (Node, error) {
	return nthcdr(2, argv[0])
}

func funCdddr(ctx context.Context, w *World, argv []Node) (Node, error) {
	return nthcdr(3, argv[0])
}

func funList(ctx context.Context, w *World, list []Node) (Node, error) {
	var cons Node = Null
	for i := len(list) - 1; i >= 0; i-- {
		cons = &Cons{
			Car: list[i],
			Cdr: cons,
		}
	}
	return cons, nil
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

func funLast(ctx context.Context, w *World, list []Node) (Node, error) {
	if IsNull(list[0]) {
		return Null, nil
	}
	tail, err := lastOfList(list[0])
	if err != nil {
		return nil, err
	}
	return tail.Car, nil
}

func funAppend(ctx context.Context, w *World, list []Node) (Node, error) {
	var last Node = Null
	for i := len(list) - 1; i >= 0; i-- {
		if IsNull(list[i]) {
			continue
		}
		tail, err := lastOfList(list[i])
		if err != nil {
			return nil, err
		}
		tail.Cdr = last
		last = list[i]
	}
	return last, nil
}

func funMember(ctx context.Context, w *World, argv []Node) (Node, error) {
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

func funCons(ctx context.Context, w *World, argv []Node) (Node, error) {
	return &Cons{Car: argv[0], Cdr: argv[1]}, nil
}

func funMapCar(ctx context.Context, w *World, argv []Node) (Node, error) {
	if len(argv) < 1 {
		return nil, ErrTooFewArguments
	}
	f, err := argv[0].Eval(ctx, w)
	if err != nil {
		return nil, err
	}
	_f, ok := f.(_Callable)
	if !ok {
		return nil, ErrExpectedFunction
	}
	listSet := argv[1:]

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

func funListp(ctx context.Context, w *World, argv []Node) (Node, error) {
	if IsNull(argv[0]) {
		return True, nil
	}
	if _, ok := argv[0].(*Cons); ok {
		return True, nil
	}
	return Null, nil
}

func funLength(ctx context.Context, w *World, argv []Node) (Node, error) {
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

func funReverse(ctx context.Context, w *World, argv []Node) (Node, error) {
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

func funAssoc(ctx context.Context, w *World, argv []Node) (Node, error) {
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

func funSubst(ctx context.Context, w *World, argv []Node) (Node, error) {
	return subst(argv[0], argv[1], argv[2]), nil
}
