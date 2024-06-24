package gmnlisp

import (
	"context"
)

// funGetCar implements (car X)
func funGetCar(_ context.Context, _ *World, argv []Node) (Node, error) {
	cons, ok := argv[0].(*Cons)
	if !ok {
		return nil, MakeError(ErrExpectedCons, argv[0])
	}
	return cons.Car, nil
}

// funGetCdr implements (cdr X)
func funGetCdr(_ context.Context, _ *World, argv []Node) (Node, error) {
	cons, ok := argv[0].(*Cons)
	if !ok {
		return nil, ErrExpectedCons
	}
	return cons.Cdr, nil
}

// funList implements (list A B ...)
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
		if IsNone(node) {
			return nil, nil
		}
		cons, ok := node.(*Cons)
		if !ok {
			return nil, MakeError(ErrExpectedCons, node)
		}
		if IsNone(cons.Cdr) {
			return cons, nil
		}
		node = cons.Cdr
	}
}

// funLast implements (last LIST)
func funLast(_ context.Context, _ *World, list []Node) (Node, error) {
	if IsNone(list[0]) {
		return Null, nil
	}
	tail, err := lastOfList(list[0])
	if err != nil {
		return nil, err
	}
	if tail == nil {
		return Null, nil
	}
	return tail.Car, nil
}

// funAppend implements (append LIST1 LIST2 ...)
func funAppend(_ context.Context, _ *World, list []Node) (Node, error) {
	var value Node
	var err error
	var buffer ListBuilder

	if len(list) <= 0 {
		return Null, nil
	}
	if len(list) == 1 {
		return list[0], nil
	}
	for _, list1 := range list[:len(list)-1] {
		for IsSome(list1) {
			value, list1, err = Shift(list1)
			if err != nil {
				return nil, err
			}
			buffer.Add(value)
		}
	}
	head := buffer.Sequence()
	tail, err := lastOfList(head)
	if err != nil {
		return nil, err
	}
	if tail == nil {
		return list[len(list)-1], nil
	}
	tail.Cdr = list[len(list)-1]
	return head, nil
}

// funCons implements (cons CAR CDR)
func funCons(_ context.Context, _ *World, argv []Node) (Node, error) {
	return &Cons{Car: argv[0], Cdr: argv[1]}, nil
}

// funLispp implements (listp VALUE)
func funListp(_ context.Context, _ *World, argv []Node) (Node, error) {
	if IsNone(argv[0]) {
		return True, nil
	}
	if _, ok := argv[0].(*Cons); ok {
		return True, nil
	}
	return Null, nil
}

// funAssoc implements (assoc KEY LIST)

func Assoc(key Node, list Node) (Node, error) {
	for IsSome(list) {
		var element Node
		var err error

		element, list, err = Shift(list)
		if err != nil {
			return nil, err
		}
		cons, ok := element.(*Cons)
		if !ok {
			return nil, ErrExpectedCons
		}
		if key.Equals(cons.Car, STRICT) {
			return cons, nil
		}
	}
	return Null, nil
}

func funAssoc(_ context.Context, _ *World, argv []Node) (Node, error) {
	return Assoc(argv[0], argv[1])
}

// funSetCar implements (set-car X Y) == (setf (car X) Y)
func funSetCar(_ context.Context, _ *World, argv []Node) (Node, error) {
	cons, ok := argv[1].(*Cons)
	if !ok {
		return nil, ErrExpectedCons
	}
	cons.Car = argv[0]
	return cons, nil
}

// funSetCdr implements (replacd X Y) == (setf (cdr X) Y)
func funSetCdr(_ context.Context, _ *World, argv []Node) (Node, error) {
	cons, ok := argv[1].(*Cons)
	if !ok {
		return nil, ErrExpectedCons
	}
	cons.Cdr = argv[0]
	return cons, nil
}

func funCreateList(_ context.Context, _ *World, argv []Node) (Node, error) {
	n, err := ExpectInteger(argv[0])
	if err != nil {
		return nil, err
	}
	result := Null
	for ; n > 0; n-- {
		result = &Cons{
			Car: argv[1],
			Cdr: result,
		}
	}
	return result, nil
}
