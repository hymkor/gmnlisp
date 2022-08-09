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

type canPlus interface {
	Node
	Add(Node) (Node, error)
}

func cmdAdd(ctx context.Context, w *World, param Node) (Node, error) {
	return w.inject(ctx, param, func(left, right Node) (Node, error) {
		if _left, ok := left.(canPlus); ok {
			return _left.Add(right)
		}
		return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(left, PRINT))
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
		return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(left, PRINT))
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
		return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(left, PRINT))
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
		return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(left, PRINT))
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
		return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(left, PRINT))
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
		return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(right, PRINT))
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
		return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(right, PRINT))
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
		return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(right, PRINT))
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

func funTruncate(ctx context.Context, w *World, argv []Node) (Node, error) {
	if value, ok := argv[0].(Integer); ok {
		return value, nil
	}
	if value, ok := argv[0].(Float); ok {
		return Integer(int(math.Trunc(float64(value)))), nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(argv[0], PRINT))
}

func funOnePlus(ctx context.Context, w *World, argv []Node) (Node, error) {
	if value, ok := argv[0].(Rune); ok {
		return value + 1, nil
	}
	if value, ok := argv[0].(Integer); ok {
		return value + 1, nil
	}
	if value, ok := argv[0].(Float); ok {
		return value + 1, nil
	}
	return nil, ErrExpectedNumber
}

func funOneMinus(ctx context.Context, w *World, argv []Node) (Node, error) {
	if value, ok := argv[0].(Rune); ok {
		return value - 1, nil
	}
	if value, ok := argv[0].(Integer); ok {
		return value - 1, nil
	}
	if value, ok := argv[0].(Float); ok {
		return value - 1, nil
	}
	return nil, ErrExpectedNumber
}

func incfDecf(ctx context.Context, w *World, list Node, f func(Symbol, Node, Node) (Node, error)) (Node, error) {
	var name Node
	var err error

	name, list, err = shift(list)
	if err != nil {
		return nil, err
	}
	var right Node = Integer(1)
	if HasValue(list) {
		right, list, err = w.shiftAndEvalCar(ctx, list)
		if HasValue(list) {
			return nil, ErrTooManyArguments
		}
	}
	symbol, ok := name.(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}
	left, err := w.Get(symbol)
	if err != nil {
		return nil, err
	}
	return f(symbol, left, right)
}

func cmdIncf(ctx context.Context, w *World, list Node) (Node, error) {
	return incfDecf(ctx, w, list, func(symbol Symbol, left, right Node) (Node, error) {
		_left, ok := left.(canPlus)
		if !ok {
			return nil, ErrNotSupportType
		}
		result, err := _left.Add(right)
		if err != nil {
			return nil, err
		}
		return result, w.Set(symbol, result)
	})
}

func cmdDecf(ctx context.Context, w *World, list Node) (Node, error) {
	return incfDecf(ctx, w, list, func(symbol Symbol, left, right Node) (Node, error) {
		_left, ok := left.(canMinus)
		if !ok {
			return nil, ErrNotSupportType
		}
		result, err := _left.Sub(right)
		if err != nil {
			return nil, err
		}
		return result, w.Set(symbol, result)
	})
}
