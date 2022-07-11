package gmnlisp

import (
	"errors"
	"fmt"
	"math"
)

var ErrNotSupportType = errors.New("Not support type")

func cmdPlus(ins *Instance, param Node) (Node, error) {
	type CanPlus interface {
		Node
		Plus(Node) (Node, error)
	}
	return ins.Inject(param, func(left, right Node) (Node, error) {
		if _left, ok := left.(CanPlus); ok {
			return _left.Plus(right)
		}
		return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(left))
	})
}

func cmdMinus(ins *Instance, param Node) (Node, error) {
	type CanMinus interface {
		Node
		Minus(Node) (Node, error)
	}
	return ins.Inject(param, func(left, right Node) (Node, error) {
		if _left, ok := left.(CanMinus); ok {
			return _left.Minus(right)
		}
		return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(left))
	})
}

func cmdMulti(ins *Instance, param Node) (Node, error) {
	type CanMulti interface {
		Node
		Multi(Node) (Node, error)
	}
	return ins.Inject(param, func(left, right Node) (Node, error) {
		if _left, ok := left.(CanMulti); ok {
			return _left.Multi(right)
		}
		return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(left))
	})
}

func cmdDevide(ins *Instance, param Node) (Node, error) {
	type CanDevide interface {
		Node
		Devide(Node) (Node, error)
	}
	return ins.Inject(param, func(left, right Node) (Node, error) {
		if _left, ok := left.(CanDevide); ok {
			return _left.Devide(right)
		}
		return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(left))
	})
}

func cmdLessThan(ins *Instance, param Node) (Node, error) {
	type CanLessThan interface {
		Node
		LessThan(Node) (Node, error)
	}
	return ins.Inject(param, func(left, right Node) (Node, error) {
		if _left, ok := left.(CanLessThan); ok {
			return _left.LessThan(right)
		}
		return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(left))
	})
}

func cmdGreaterThan(ins *Instance, param Node) (Node, error) {
	type CanLessThan interface {
		Node
		LessThan(Node) (Node, error)
	}
	return ins.Inject(param, func(left, right Node) (Node, error) {
		if _right, ok := right.(CanLessThan); ok {
			return _right.LessThan(left)
		}
		return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(right))
	})
}

func not(n Node, err error) (Node, error) {
	if IsNull(n) {
		return True, err
	}
	return Null, err
}

func cmdGreaterOrEqual(ins *Instance, param Node) (Node, error) {
	type CanLessThan interface {
		Node
		LessThan(Node) (Node, error)
	}
	return ins.Inject(param, func(left, right Node) (Node, error) {
		//     left >= right
		// <=> not (left < right )
		if _left, ok := left.(CanLessThan); ok {
			return not(_left.LessThan(right))
		}
		return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(right))
	})
}

func cmdLessOrEqual(ins *Instance, param Node) (Node, error) {
	type CanLessThan interface {
		Node
		LessThan(Node) (Node, error)
	}
	return ins.Inject(param, func(left, right Node) (Node, error) {
		//     left <= right
		// <=> not (right < left )
		if _right, ok := right.(CanLessThan); ok {
			return not(_right.LessThan(left))
		}
		return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(right))
	})
}

func cmdTruncate(ins *Instance, this Node) (Node, error) {
	first, _, err := ins.ShiftAndEvalCar(this)
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
