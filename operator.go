package gommon

import (
	"errors"
	"fmt"
)

var ErrNotSupportType = errors.New("Not support type")

func inject[T Node](this Node, name string, f func(left, right T) (Node, error)) (Node, error) {
	var result T
	var _f func(left, right T) (Node, error)

	_f = func(left, right T) (Node, error) {
		_f = f
		return right, nil
	}
	err := ForEachEval(this, func(one Node) error {
		value, ok := one.(T)
		if !ok {
			return fmt.Errorf("%s: %w %s",
				name,
				ErrExpectedNumber,
				Node2String(one))
		}
		result1, err1 := _f(result, value)
		if err1 != nil {
			return err1
		}
		result, ok = result1.(T)
		if !ok {
			return ErrNotSupportType
		}
		return nil
	})
	return result, err
}

type CanPlus interface {
	Node
	Plus(Node) (Node, error)
}

func CmdPlus(this Node) (Node, error) {
	return inject(this, "+", func(left, right CanPlus) (Node, error) {
		rv, err := left.Plus(right)
		return Node(rv), err
	})
}

type CanMinus interface {
	Node
	Minus(Node) (Node, error)
}

func CmdMinus(this Node) (Node, error) {
	return inject(this, "-", func(left, right CanMinus) (Node, error) {
		return left.Minus(right)
	})
}

type CanMulti interface {
	Node
	Multi(Node) (Node, error)
}

func CmdMulti(this Node) (Node, error) {
	return inject(this, "*", func(left, right CanMulti) (Node, error) {
		return left.Multi(right)
	})
}

type CanDevide interface {
	Node
	Devide(Node) (Node, error)
}

func CmdDevide(this Node) (Node, error) {
	return inject(this, "/", func(left, right CanDevide) (Node, error) {
		return left.Devide(right)
	})
}
