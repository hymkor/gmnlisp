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
	Add(Node) (Node, error)
}

func cmdAdd(ctx context.Context, w *World, param Node) (Node, error) {
	return w.inject(ctx, param, func(left, right Node) (Node, error) {
		if _left, ok := left.(canPlus); ok {
			return _left.Add(right)
		}
		return nil, MakeError(ErrNotSupportType, left)
	})
}

type canMinus interface {
	Node
	Sub(Node) (Node, error)
}

func cmdSub(ctx context.Context, w *World, param Node) (Node, error) {
	return w.inject(ctx, param, func(left, right Node) (Node, error) {
		if _left, ok := left.(canMinus); ok {
			return _left.Sub(right)
		}
		return nil, MakeError(ErrNotSupportType, left)
	})
}

func cmdMulti(ctx context.Context, w *World, param Node) (Node, error) {
	type CanMulti interface {
		Node
		Multi(Node) (Node, error)
	}
	return w.inject(ctx, param, func(left, right Node) (Node, error) {
		if _left, ok := left.(CanMulti); ok {
			return _left.Multi(right)
		}
		return nil, MakeError(ErrNotSupportType, left)
	})
}

func cmdDevide(ctx context.Context, w *World, param Node) (Node, error) {
	type CanDevide interface {
		Node
		Divide(Node) (Node, error)
	}
	return w.inject(ctx, param, func(left, right Node) (Node, error) {
		if _left, ok := left.(CanDevide); ok {
			return _left.Divide(right)
		}
		return nil, MakeError(ErrNotSupportType, left)
	})
}

type canLessThan interface {
	LessThan(Node) (bool, error)
}

func cmdLessThan(ctx context.Context, w *World, param Node) (Node, error) {
	return notNullToTrue(w.inject(ctx, param, func(left, right Node) (Node, error) {
		if _left, ok := left.(canLessThan); ok {
			result, err := _left.LessThan(right)
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

func cmdGreaterThan(ctx context.Context, w *World, param Node) (Node, error) {
	return notNullToTrue(w.inject(ctx, param, func(left, right Node) (Node, error) {
		if _right, ok := right.(canLessThan); ok {
			result, err := _right.LessThan(left)
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

func cmdEqualOp(ctx context.Context, w *World, param Node) (Node, error) {
	return notNullToTrue(w.inject(ctx, param, func(left, right Node) (Node, error) {
		if left.Equals(right, EQUALP) {
			return right, nil
		}
		return Null, nil
	}))
}

func cmdGreaterOrEqual(ctx context.Context, w *World, param Node) (Node, error) {
	return notNullToTrue(w.inject(ctx, param, func(left, right Node) (Node, error) {
		//     left >= right
		// <=> not (left < right )
		if _left, ok := left.(canLessThan); ok {
			result, err := _left.LessThan(right)
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

func cmdLessOrEqual(ctx context.Context, w *World, param Node) (Node, error) {
	return notNullToTrue(w.inject(ctx, param, func(left, right Node) (Node, error) {
		//     left <= right
		// <=> not (right < left)
		if _right, ok := right.(canLessThan); ok {
			result, err := _right.LessThan(left)
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

func floatToInteger(argv []Node, f func(float64) float64) (Node, error) {
	if value, ok := argv[0].(Integer); ok {
		return value, nil
	}
	if value, ok := argv[0].(Float); ok {
		return Integer(int(f(float64(value)))), nil
	}
	return nil, MakeError(ErrNotSupportType, argv[0])
}

// funTruncate implements (truncte X). It returns the integer value of X.
func funTruncate(ctx context.Context, w *World, argv []Node) (Node, error) {
	return floatToInteger(argv, math.Trunc)
}

// funFloor implements (truncte X). It returns the greatest integer value less than or equal to x.
func funFloor(ctx context.Context, w *World, argv []Node) (Node, error) {
	return floatToInteger(argv, math.Floor)
}

// funCeiling implements (ceiling X). It returns the least integer value greater than or equal to x.
func funCeiling(ctx context.Context, w *World, argv []Node) (Node, error) {
	return floatToInteger(argv, math.Ceil)
}

func funRound(ctx context.Context, w *World, argv []Node) (Node, error) {
	return floatToInteger(argv, math.Round)
}

func funMod(ctx context.Context, w *World, list []Node) (Node, error) {
	var left float64
	var right float64
	bits := 0

	if _left, ok := list[0].(Float); ok {
		left = float64(_left)
	} else {
		_left, ok := list[0].(Integer)
		if !ok {
			return nil, ErrNotSupportType
		}
		left = float64(int(_left))
		bits = 1
	}
	if _right, ok := list[1].(Float); ok {
		right = float64(_right)
	} else {
		_right, ok := list[1].(Integer)
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

func funRem(ctx context.Context, w *World, list []Node) (Node, error) {
	if left, ok := list[0].(Integer); ok {
		if right, ok := list[1].(Integer); ok {
			return Integer(left % right), nil
		}
	}
	if left, ok := list[0].(Float); ok {
		if right, ok := list[1].(Float); ok {
			return Float(math.Mod(float64(left), float64(right))), nil
		}
		if right, ok := list[1].(Integer); ok {
			return Float(math.Mod(float64(left), float64(int(right)))), nil
		}
	}
	return nil, ErrNotSupportType
}
