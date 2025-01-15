package gmnlisp

import (
	"context"
)

// funGetCar implements (car X)
func funGetCar(ctx context.Context, w *World, arg Node) (Node, error) {
	cons, err := ExpectClass[*Cons](ctx, w, arg)
	if err != nil {
		return nil, err
	}
	return cons.Car, nil
}

// funGetCdr implements (cdr X)
func funGetCdr(ctx context.Context, w *World, arg Node) (Node, error) {
	cons, err := ExpectClass[*Cons](ctx, w, arg)
	if err != nil {
		return nil, err
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

func lastOfList(ctx context.Context, w *World, node Node) (*Cons, error) {
	for {
		if IsNone(node) {
			return nil, nil
		}
		cons, err := ExpectClass[*Cons](ctx, w, node)
		if err != nil {
			return nil, err
		}
		if IsNone(cons.Cdr) {
			return cons, nil
		}
		node = cons.Cdr
	}
}

// funLast implements (last LIST)
func funLast(ctx context.Context, w *World, arg Node) (Node, error) {
	if IsNone(arg) {
		return Null, nil
	}
	tail, err := lastOfList(ctx, w, arg)
	if err != nil {
		return nil, err
	}
	if tail == nil {
		return Null, nil
	}
	return tail.Car, nil
}

// funAppend implements (append LIST1 LIST2 ...)
func funAppend(ctx context.Context, w *World, list []Node) (Node, error) {
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
			buffer.Add(ctx, w, value)
		}
	}
	head := buffer.Sequence()
	tail, err := lastOfList(ctx, w, head)
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
func funCons(_ context.Context, _ *World, first, second Node) (Node, error) {
	return &Cons{Car: first, Cdr: second}, nil
}

// funLispp implements (listp VALUE)
func funListp(_ context.Context, _ *World, arg Node) (Node, error) {
	if IsNone(arg) {
		return True, nil
	}
	if _, ok := arg.(*Cons); ok {
		return True, nil
	}
	return Null, nil
}

// funAssoc implements (assoc KEY LIST)

func Assoc(ctx context.Context, w *World, key Node, list Node) (Node, error) {
	for IsSome(list) {
		var element Node
		var err error

		element, list, err = Shift(list)
		if err != nil {
			return nil, err
		}
		cons, err := ExpectClass[*Cons](ctx, w, element)
		if err != nil {
			return nil, err
		}
		if key.Equals(cons.Car, STRICT) {
			return cons, nil
		}
	}
	return Null, nil
}

// funSetCar implements (set-car X Y) == (setf (car Y) X)
func funSetCar(ctx context.Context, w *World, first, second Node) (Node, error) {
	cons, err := ExpectClass[*Cons](ctx, w, second)
	if err != nil {
		return nil, err
	}
	cons.Car = first
	return first, nil
}

// funSetCdr implements (replacd X Y) == (setf (cdr X) Y) = (set-cdr Y X)
func funSetCdr(ctx context.Context, w *World, first, second Node) (Node, error) {
	cons, err := ExpectClass[*Cons](ctx, w, second)
	if err != nil {
		return nil, err
	}
	cons.Cdr = first
	return first, nil
}

func funCreateList(ctx context.Context, w *World, argv []Node) (Node, error) {
	n, err := ExpectClass[Integer](ctx, w, argv[0])
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
