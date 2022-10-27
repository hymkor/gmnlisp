package common

import (
	"context"

	. "github.com/hymkor/gmnlisp"
)

func nthcdr(n int, list Node) (Node, error) {
	for i := 0; i < n; i++ {
		cons, ok := list.(*Cons)
		if !ok {
			return nil, ErrExpectedCons
		}
		list = cons.Cdr
	}
	return list, nil
}

// funNthcdr implements (nthcdr N LIST) and (setf (nth N LIST) VALUE)
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
		return nil, ErrExpectedCons
	}
	return cons.Car, nil
}

// funNthcdr implements (nth N LIST) and (setf (nth N LIST) VALUE)
func funNth(_ context.Context, _ *World, argv []Node) (Node, error) {
	n, ok := argv[0].(Integer)
	if !ok {
		return nil, ErrExpectedNumber
	}
	return nth(int(n), argv[1])
}

// funCadr implements (cadr LIST) and (setf (cadr LIST) VALUE)
func funCadr(_ context.Context, _ *World, argv []Node) (Node, error) {
	return nth(1, argv[0])
}

// funCaddr implements (caddr LIST) and (setf (caddr LIST) VALUE)
func funCaddr(_ context.Context, _ *World, argv []Node) (Node, error) {
	return nth(2, argv[0])
}

// funCadddr implements (cadddr LIST) and (setf (cadddr LIST) VALUE)
func funCadddr(_ context.Context, _ *World, argv []Node) (Node, error) {
	return nth(3, argv[0])
}

// funCddr implements (cddr LIST) and (setf (cddr LIST) VALUE)
func funCddr(_ context.Context, _ *World, argv []Node) (Node, error) {
	return nthcdr(2, argv[0])
}

// funCdddr implements (cdddr LIST) and (setf (cdddr LIST) VALUE)
func funCdddr(_ context.Context, _ *World, argv []Node) (Node, error) {
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
