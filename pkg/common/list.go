package common

import (
	"context"

	. "github.com/hymkor/gmnlisp"
)

func nthcdr(n int, list Node) (Node, func(Node) error, error) {
	var setter = func(value Node) error { return nil }
	for i := 0; i < n; i++ {
		cons, ok := list.(*Cons)
		if !ok {
			return nil, nil, ErrExpectedCons
		}
		list = cons.Cdr
		setter = func(value Node) error { cons.Cdr = value; return nil }
	}
	return list, setter, nil
}

// funNthcdr implements (nthcdr N LIST) and (setf (nth N LIST) VALUE)
func funNthcdr(_ context.Context, _ *World, argv []Node) (Node, func(Node) error, error) {
	n, ok := argv[0].(Integer)
	if !ok {
		return nil, nil, ErrExpectedNumber
	}
	return nthcdr(int(n), argv[1])
}

func nth(n int, list Node) (Node, func(Node) error, error) {
	var err error

	list, _, err = nthcdr(n, list)
	if err != nil {
		return nil, nil, err
	}
	cons, ok := list.(*Cons)
	if !ok {
		return nil, nil, ErrExpectedCons
	}
	return cons.Car, func(value Node) error { cons.Car = value; return nil }, nil
}

// funNthcdr implements (nth N LIST) and (setf (nth N LIST) VALUE)
func funNth(_ context.Context, _ *World, argv []Node) (Node, func(Node) error, error) {
	n, ok := argv[0].(Integer)
	if !ok {
		return nil, nil, ErrExpectedNumber
	}
	return nth(int(n), argv[1])
}

// funCadr implements (cadr LIST) and (setf (cadr LIST) VALUE)
func funCadr(_ context.Context, _ *World, argv []Node) (Node, func(Node) error, error) {
	return nth(1, argv[0])
}

// funCaddr implements (caddr LIST) and (setf (caddr LIST) VALUE)
func funCaddr(_ context.Context, _ *World, argv []Node) (Node, func(Node) error, error) {
	return nth(2, argv[0])
}

// funCadddr implements (cadddr LIST) and (setf (cadddr LIST) VALUE)
func funCadddr(_ context.Context, _ *World, argv []Node) (Node, func(Node) error, error) {
	return nth(3, argv[0])
}

// funCddr implements (cddr LIST) and (setf (cddr LIST) VALUE)
func funCddr(_ context.Context, _ *World, argv []Node) (Node, func(Node) error, error) {
	return nthcdr(2, argv[0])
}

// funCdddr implements (cdddr LIST) and (setf (cdddr LIST) VALUE)
func funCdddr(_ context.Context, _ *World, argv []Node) (Node, func(Node) error, error) {
	return nthcdr(3, argv[0])
}

func subst(newItem, oldItem, list Node) Node {
	if list.Equals(oldItem, STRICT) {
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

// funSubst implements (subst NEWITEM OLDITEM LIST)
func funSubst(_ context.Context, _ *World, argv []Node) (Node, error) {
	return subst(argv[0], argv[1], argv[2]), nil
}
