package gmnlisp

import (
	"context"
	"fmt"
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

func cmdAdd(ctx context.Context, w *World, param Node) (Node, error) {
	type CanPlus interface {
		Node
		Add(Node) (Node, error)
	}
	return w.inject(ctx, param, func(left, right Node) (Node, error) {
		if _left, ok := left.(CanPlus); ok {
			return _left.Add(right)
		}
		return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(left))
	})
}

func cmdSub(ctx context.Context, w *World, param Node) (Node, error) {
	type CanMinus interface {
		Node
		Sub(Node) (Node, error)
	}
	return w.inject(ctx, param, func(left, right Node) (Node, error) {
		if _left, ok := left.(CanMinus); ok {
			return _left.Sub(right)
		}
		return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(left))
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
		return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(left))
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
		return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(left))
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
		return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(left))
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
		return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(right))
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
		return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(right))
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
		return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(right))
	}))
}

func cmdAnd(ctx context.Context, w *World, param Node) (Node, error) {
	for {
		var value Node
		var err error

		value, param, err = w.shiftAndEvalCar(ctx, param)
		if err != nil {
			return nil, err
		}
		if IsNull(value) {
			return Null, nil
		}
		if IsNull(param) {
			return value, nil
		}
	}
}

func cmdOr(ctx context.Context, w *World, param Node) (Node, error) {
	for {
		var value Node
		var err error

		value, param, err = w.shiftAndEvalCar(ctx, param)
		if err != nil {
			return nil, err
		}
		if HasValue(value) {
			return value, nil
		}
		if IsNull(param) {
			return Null, nil
		}
	}
}

func cmdTruncate(ctx context.Context, w *World, argv []Node) (Node, error) {
	if value, ok := argv[0].(Integer); ok {
		return value, nil
	}
	if value, ok := argv[0].(Float); ok {
		return Integer(int(math.Trunc(float64(value)))), nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(argv[0]))
}
