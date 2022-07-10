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
