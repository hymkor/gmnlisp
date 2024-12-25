package gmnlisp

import (
	"context"
	"math"
)

func notNullToTrue(v Node, err error) (Node, error) {
	if v == nil || err != nil {
		return Null, err
	}
	if _, ok := v.(_NullType); ok {
		return Null, err
	}
	return True, err
}

type canPlus interface {
	Node
	Add(context.Context, *World, Node) (Node, error)
}

func funAdd(ctx context.Context, w *World, args []Node) (Node, error) {
	return inject(args, func(left, right Node) (Node, error) {
		_left, err := ExpectInterface[canPlus](ctx, w, left, floatClass)
		if err != nil {
			return nil, err
		}
		return _left.Add(ctx, w, right)
	})
}

type canMinus interface {
	Node
	Sub(context.Context, *World, Node) (Node, error)
}

func funSub(ctx context.Context, w *World, args []Node) (Node, error) {
	if len(args) == 1 {
		class := args[0].ClassOf()
		zero := class.Create()
		z, err := ExpectInterface[canMinus](ctx, w, zero, class)
		if err != nil {
			return nil, err
		}
		return z.Sub(ctx, w, args[0])
	}
	return inject(args, func(left, right Node) (Node, error) {
		_left, err := ExpectInterface[canMinus](ctx, w, left, floatClass)
		if err != nil {
			return nil, err
		}
		return _left.Sub(ctx, w, right)
	})
}

func funMulti(ctx context.Context, w *World, args []Node) (Node, error) {
	type CanMulti interface {
		Node
		Multi(context.Context, *World, Node) (Node, error)
	}
	return inject(args, func(left, right Node) (Node, error) {
		_left, err := ExpectInterface[CanMulti](ctx, w, left, floatClass)
		if err != nil {
			return nil, err
		}
		return _left.Multi(ctx, w, right)
	})
}

func funDevide(ctx context.Context, w *World, args []Node) (Node, error) {
	type CanDevide interface {
		Node
		Divide(context.Context, *World, Node) (Node, error)
	}
	return inject(args, func(left, right Node) (Node, error) {
		_left, err := ExpectInterface[CanDevide](ctx, w, left, floatClass)
		if err != nil {
			return nil, err
		}
		return _left.Divide(ctx, w, right)
	})
}

type canLessThan interface {
	LessThan(context.Context, *World, Node) (bool, error)
	Node
}

func funLessThan(ctx context.Context, w *World, args []Node) (Node, error) {
	return notNullToTrue(inject(args, func(left, right Node) (Node, error) {
		_left, err := ExpectInterface[canLessThan](ctx, w, left, floatClass)
		if err != nil {
			return nil, err
		}
		result, err := _left.LessThan(ctx, w, right)
		if err != nil {
			return Null, err
		}
		if result {
			return right, nil
		}
		return Null, nil
	}))
}

func funGreaterThan(ctx context.Context, w *World, args []Node) (Node, error) {
	return notNullToTrue(inject(args, func(left, right Node) (Node, error) {
		_right, err := ExpectInterface[canLessThan](ctx, w, right, floatClass)
		if err != nil {
			return nil, err
		}
		result, err := _right.LessThan(ctx, w, left)
		if err != nil {
			return Null, err
		}
		if result {
			return right, nil
		}
		return Null, nil
	}))
}

func funEqualOp(ctx context.Context, w *World, args []Node) (Node, error) {
	return notNullToTrue(inject(args, func(left, right Node) (Node, error) {
		if left.Equals(right, EQUALP) {
			return right, nil
		}
		return Null, nil
	}))
}

func funGreaterOrEqual(ctx context.Context, w *World, args []Node) (Node, error) {
	return notNullToTrue(inject(args, func(left, right Node) (Node, error) {
		//     left >= right
		// <=> not (left < right )
		_left, err := ExpectInterface[canLessThan](ctx, w, left, floatClass)
		if err != nil {
			return nil, err
		}
		result, err := _left.LessThan(ctx, w, right)
		if err != nil {
			return Null, err
		}
		if result {
			return Null, nil
		}
		return right, nil
	}))
}

func funLessOrEqual(ctx context.Context, w *World, args []Node) (Node, error) {
	return notNullToTrue(inject(args, func(left, right Node) (Node, error) {
		//     left <= right
		// <=> not (right < left)
		_right, err := ExpectInterface[canLessThan](ctx, w, right, floatClass)
		if err != nil {
			return nil, err
		}
		result, err := _right.LessThan(ctx, w, left)
		if err != nil {
			return Null, err
		}
		if result {
			return Null, nil
		}
		return right, nil
	}))
}

func cmdAnd(ctx context.Context, w *World, param Node) (Node, error) {
	for {
		var value Node
		var err error

		value, param, err = w.ShiftAndEvalCar(ctx, param)
		if err != nil {
			return nil, err
		}
		if IsNone(value) {
			return Null, nil
		}
		if IsNone(param) {
			return value, nil
		}
	}
}

func cmdOr(ctx context.Context, w *World, param Node) (Node, error) {
	for {
		var value Node
		var err error

		value, param, err = w.ShiftAndEvalCar(ctx, param)
		if err != nil {
			return nil, err
		}
		if IsSome(value) {
			return value, nil
		}
		if IsNone(param) {
			return Null, nil
		}
	}
}

func floatToInteger(ctx context.Context, w *World, arg Node, f func(float64) float64) (Node, error) {
	if value, ok := arg.(Integer); ok {
		return value, nil
	}
	value, err := ExpectClass[Float](ctx, w, arg)
	if err != nil {
		return nil, err
	}
	return Integer(int(f(float64(value)))), nil
}

// funTruncate implements (truncte X). It returns the integer value of X.
func funTruncate(ctx context.Context, w *World, arg Node) (Node, error) {
	return floatToInteger(ctx, w, arg, math.Trunc)
}

// funFloor implements (truncte X). It returns the greatest integer value less than or equal to x.
func funFloor(ctx context.Context, w *World, arg Node) (Node, error) {
	return floatToInteger(ctx, w, arg, math.Floor)
}

// funCeiling implements (ceiling X). It returns the least integer value greater than or equal to x.
func funCeiling(ctx context.Context, w *World, arg Node) (Node, error) {
	return floatToInteger(ctx, w, arg, math.Ceil)
}

func funRound(ctx context.Context, w *World, arg Node) (Node, error) {
	return floatToInteger(ctx, w, arg, math.Round)
}

func funMod(ctx context.Context, w *World, first, second Node) (Node, error) {
	var left float64
	var right float64
	bits := 0

	if _left, ok := first.(Integer); ok {
		left = float64(int(_left))
		bits = 1
	} else if _left, err := ExpectClass[Float](ctx, w, first); err != nil {
		return nil, err
	} else {
		left = float64(_left)
	}

	if _right, ok := second.(Integer); ok {
		right = float64(int(_right))
		bits |= 2
	} else if _right, err := ExpectClass[Float](ctx, w, second); err != nil {
		return nil, err
	} else {
		right = float64(_right)
	}
	value := math.Remainder(left, right)
	if bits == 3 {
		return Integer(int(value)), nil
	}
	return Float(value), nil
}

func funRem(ctx context.Context, w *World, first, second Node) (Node, error) {
	if left, ok := first.(Integer); ok {
		if right, ok := second.(Integer); ok {
			return Integer(left % right), nil
		}
	}
	left, err := ExpectClass[Float](ctx, w, first)
	if err != nil {
		return nil, err
	}
	if right, ok := second.(Integer); ok {
		return Float(math.Mod(float64(left), float64(int(right)))), nil
	}
	right, err := ExpectClass[Float](ctx, w, second)
	if err != nil {
		return nil, err
	}
	return Float(math.Mod(float64(left), float64(right))), nil
}
