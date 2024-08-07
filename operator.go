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
		if _left, ok := left.(canPlus); ok {
			return _left.Add(ctx, w, right)
		}
		return nil, MakeError(ErrNotSupportType, left)
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
		if _left, ok := left.(canMinus); ok {
			return _left.Sub(ctx, w, right)
		}
		return nil, MakeError(ErrNotSupportType, left)
	})
}

func funMulti(ctx context.Context, w *World, args []Node) (Node, error) {
	type CanMulti interface {
		Node
		Multi(context.Context, *World, Node) (Node, error)
	}
	return inject(args, func(left, right Node) (Node, error) {
		if _left, ok := left.(CanMulti); ok {
			return _left.Multi(ctx, w, right)
		}
		return nil, MakeError(ErrNotSupportType, left)
	})
}

func funDevide(ctx context.Context, w *World, args []Node) (Node, error) {
	type CanDevide interface {
		Node
		Divide(context.Context, *World, Node) (Node, error)
	}
	return inject(args, func(left, right Node) (Node, error) {
		if _left, ok := left.(CanDevide); ok {
			return _left.Divide(ctx, w, right)
		}
		return nil, MakeError(ErrNotSupportType, left)
	})
}

type canLessThan interface {
	LessThan(context.Context, *World, Node) (bool, error)
}

func funLessThan(ctx context.Context, w *World, args []Node) (Node, error) {
	return notNullToTrue(inject(args, func(left, right Node) (Node, error) {
		if _left, ok := left.(canLessThan); ok {
			result, err := _left.LessThan(ctx, w, right)
			if err != nil {
				return Null, err
			}
			if result {
				return right, nil
			}
			return Null, nil
		}
		return nil, MakeError(ErrNotSupportType, left)
	}))
}

func funGreaterThan(ctx context.Context, w *World, args []Node) (Node, error) {
	return notNullToTrue(inject(args, func(left, right Node) (Node, error) {
		if _right, ok := right.(canLessThan); ok {
			result, err := _right.LessThan(ctx, w, left)
			if err != nil {
				return Null, err
			}
			if result {
				return right, nil
			}
			return Null, nil
		}
		return nil, MakeError(ErrNotSupportType, right)
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
		if _left, ok := left.(canLessThan); ok {
			result, err := _left.LessThan(ctx, w, right)
			if err != nil {
				return Null, err
			}
			if result {
				return Null, nil
			}
			return right, nil
		}
		return nil, MakeError(ErrNotSupportType, right)
	}))
}

func funLessOrEqual(ctx context.Context, w *World, args []Node) (Node, error) {
	return notNullToTrue(inject(args, func(left, right Node) (Node, error) {
		//     left <= right
		// <=> not (right < left)
		if _right, ok := right.(canLessThan); ok {
			result, err := _right.LessThan(ctx, w, left)
			if err != nil {
				return Null, err
			}
			if result {
				return Null, nil
			}
			return right, nil
		}
		return nil, MakeError(ErrNotSupportType, right)
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

func floatToInteger(arg Node, f func(float64) float64) (Node, error) {
	if value, ok := arg.(Integer); ok {
		return value, nil
	}
	if value, ok := arg.(Float); ok {
		return Integer(int(f(float64(value)))), nil
	}
	return nil, MakeError(ErrNotSupportType, arg)
}

// funTruncate implements (truncte X). It returns the integer value of X.
func funTruncate(ctx context.Context, w *World, arg Node) (Node, error) {
	return floatToInteger(arg, math.Trunc)
}

// funFloor implements (truncte X). It returns the greatest integer value less than or equal to x.
func funFloor(ctx context.Context, w *World, arg Node) (Node, error) {
	return floatToInteger(arg, math.Floor)
}

// funCeiling implements (ceiling X). It returns the least integer value greater than or equal to x.
func funCeiling(ctx context.Context, w *World, arg Node) (Node, error) {
	return floatToInteger(arg, math.Ceil)
}

func funRound(ctx context.Context, w *World, arg Node) (Node, error) {
	return floatToInteger(arg, math.Round)
}

func funMod(ctx context.Context, w *World, first, second Node) (Node, error) {
	var left float64
	var right float64
	bits := 0

	if _left, ok := first.(Float); ok {
		left = float64(_left)
	} else {
		_left, ok := first.(Integer)
		if !ok {
			return nil, ErrNotSupportType
		}
		left = float64(int(_left))
		bits = 1
	}
	if _right, ok := second.(Float); ok {
		right = float64(_right)
	} else {
		_right, ok := second.(Integer)
		if !ok {
			return nil, ErrNotSupportType
		}
		right = float64(int(_right))
		bits |= 2
	}
	value := math.Remainder(float64(left), float64(right))
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
	if left, ok := first.(Float); ok {
		if right, ok := second.(Float); ok {
			return Float(math.Mod(float64(left), float64(right))), nil
		}
		if right, ok := second.(Integer); ok {
			return Float(math.Mod(float64(left), float64(int(right)))), nil
		}
	}
	return nil, ErrNotSupportType
}
