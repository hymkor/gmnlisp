package gmnlisp

import (
	"context"
	"fmt"
)

func funCar(_ context.Context, _ *World, argv []Node) (Node, error) {
	cons, ok := argv[0].(*Cons)
	if !ok {
		return nil, fmt.Errorf("%w: %s", ErrExpectedCons, toString(argv[0], PRINT))
	}
	return cons.Car, nil
}

func funCdr(_ context.Context, _ *World, argv []Node) (Node, error) {
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

func funCadr(_ context.Context, _ *World, argv []Node) (Node, error) {
	return nth(1, argv[0])
}

func funCaddr(_ context.Context, _ *World, argv []Node) (Node, error) {
	return nth(2, argv[0])
}

func funCadddr(_ context.Context, _ *World, argv []Node) (Node, error) {
	return nth(3, argv[0])
}

func funCddr(_ context.Context, _ *World, argv []Node) (Node, error) {
	return nthcdr(2, argv[0])
}

func funCdddr(_ context.Context, _ *World, argv []Node) (Node, error) {
	return nthcdr(3, argv[0])
}

func funList(_ context.Context, _ *World, list []Node) (Node, error) {
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

func funLast(_ context.Context, _ *World, list []Node) (Node, error) {
	if IsNull(list[0]) {
		return Null, nil
	}
	tail, err := lastOfList(list[0])
	if err != nil {
		return nil, err
	}
	return tail.Car, nil
}

func funAppend(_ context.Context, _ *World, list []Node) (Node, error) {
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

func funMember(_ context.Context, _ *World, argv []Node) (Node, error) {
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

func funCons(_ context.Context, _ *World, argv []Node) (Node, error) {
	return &Cons{Car: argv[0], Cdr: argv[1]}, nil
}

func funListp(_ context.Context, _ *World, argv []Node) (Node, error) {
	if IsNull(argv[0]) {
		return True, nil
	}
	if _, ok := argv[0].(*Cons); ok {
		return True, nil
	}
	return Null, nil
}

func funAssoc(_ context.Context, _ *World, argv []Node) (Node, error) {
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

func funSubst(_ context.Context, _ *World, argv []Node) (Node, error) {
	return subst(argv[0], argv[1], argv[2]), nil
}
