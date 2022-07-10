package gmnlisp

import (
	"errors"
	"fmt"
	"math"
)

var ErrNotSupportType = errors.New("Not support type")

func Inject(this Node, f func(left, right Node) (Node, error)) (Node, error) {
	result, rest, err := ShiftAndEvalCar(this)
	if err != nil {
		return nil, err
	}
	for HasValue(rest) {
		var next Node
		var err error

		next, rest, err = ShiftAndEvalCar(rest)
		if err != nil {
			return nil, err
		}
		result, err = f(result, next)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

type CanPlus interface {
	Node
	Plus(Node) (Node, error)
}

func CmdPlus(this Node) (Node, error) {
	return Inject(this, func(left, right Node) (Node, error) {
		if _left, ok := left.(CanPlus); ok {
			return _left.Plus(right)
		}
		return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, Node2String(left))
	})
}

type CanMinus interface {
	Node
	Minus(Node) (Node, error)
}

func CmdMinus(this Node) (Node, error) {
	return Inject(this, func(left, right Node) (Node, error) {
		if _left, ok := left.(CanMinus); ok {
			return _left.Minus(right)
		}
		return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, Node2String(left))
	})
}

type CanMulti interface {
	Node
	Multi(Node) (Node, error)
}

func CmdMulti(this Node) (Node, error) {
	return Inject(this, func(left, right Node) (Node, error) {
		if _left, ok := left.(CanMulti); ok {
			return _left.Multi(right)
		}
		return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, Node2String(left))
	})
}

type CanDevide interface {
	Node
	Devide(Node) (Node, error)
}

func CmdDevide(this Node) (Node, error) {
	return Inject(this, func(left, right Node) (Node, error) {
		if _left, ok := left.(CanDevide); ok {
			return _left.Devide(right)
		}
		return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, Node2String(left))
	})
}

func CmdTruncate(this Node) (Node, error) {
	first, _, err := ShiftAndEvalCar(this)
	if err != nil {
		return nil, err
	}
	if value, ok := first.(NodeInteger); ok {
		return value, err
	}
	if value, ok := first.(NodeFloat); ok {
		return NodeInteger(int(math.Trunc(float64(value)))), nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, Node2String(first))
}
