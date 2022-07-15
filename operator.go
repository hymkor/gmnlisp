package gmnlisp

import (
	"errors"
	"fmt"
	"math"
)

var ErrNotSupportType = errors.New("Not support type")

func notNullToTrue(v Node, err error) (Node, error) {
	if v == nil || err != nil {
		return Null, err
	}
	if _, ok := v.(_NullType); ok {
		return Null, err
	}
	return True, err
}

func cmdPlus(w *World, param Node) (Node, error) {
	type CanPlus interface {
		Node
		Add(Node) (Node, error)
	}
	return w.inject(param, func(left, right Node) (Node, error) {
		if _left, ok := left.(CanPlus); ok {
			return _left.Add(right)
		}
		return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(left))
	})
}

func cmdMinus(w *World, param Node) (Node, error) {
	type CanMinus interface {
		Node
		Sub(Node) (Node, error)
	}
	return w.inject(param, func(left, right Node) (Node, error) {
		if _left, ok := left.(CanMinus); ok {
			return _left.Sub(right)
		}
		return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(left))
	})
}

func cmdMulti(w *World, param Node) (Node, error) {
	type CanMulti interface {
		Node
		Multi(Node) (Node, error)
	}
	return w.inject(param, func(left, right Node) (Node, error) {
		if _left, ok := left.(CanMulti); ok {
			return _left.Multi(right)
		}
		return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(left))
	})
}

func cmdDevide(w *World, param Node) (Node, error) {
	type CanDevide interface {
		Node
		Devide(Node) (Node, error)
	}
	return w.inject(param, func(left, right Node) (Node, error) {
		if _left, ok := left.(CanDevide); ok {
			return _left.Devide(right)
		}
		return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(left))
	})
}

type canLessThan interface {
	LessThan(Node) (bool, error)
}

func cmdLessThan(w *World, param Node) (Node, error) {
	return notNullToTrue(w.inject(param, func(left, right Node) (Node, error) {
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

func cmdGreaterThan(w *World, param Node) (Node, error) {
	return notNullToTrue(w.inject(param, func(left, right Node) (Node, error) {
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

func cmdEqualOp(w *World, param Node) (Node, error) {
	type CanEqualP interface {
		EqualP(Node) bool
	}
	return notNullToTrue(w.inject(param, func(left, right Node) (Node, error) {
		if _left, ok := left.(CanEqualP); ok {
			if _left.EqualP(right) {
				return right, nil
			}
			return Null, nil
		}
		return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(right))
	}))
}

func cmdGreaterOrEqual(w *World, param Node) (Node, error) {
	return notNullToTrue(w.inject(param, func(left, right Node) (Node, error) {
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

func cmdLessOrEqual(w *World, param Node) (Node, error) {
	return notNullToTrue(w.inject(param, func(left, right Node) (Node, error) {
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

func cmdTruncate(w *World, this Node) (Node, error) {
	first, _, err := w.shiftAndEvalCar(this)
	if err != nil {
		return nil, err
	}
	if value, ok := first.(Integer); ok {
		return value, err
	}
	if value, ok := first.(Float); ok {
		return Integer(int(math.Trunc(float64(value)))), nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(first))
}
