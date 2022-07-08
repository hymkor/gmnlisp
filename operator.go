package gommon

import (
	"errors"
	"fmt"
)

var ErrNotSupportType = errors.New("Not support type")

func inject[T Node](this Node, name string, f func(left, right T) T) (Node, error) {
	var result T
	var _f func(left, right T) T

	_f = func(left, right T) T {
		_f = f
		return right
	}
	err := ForEachEval(this, func(one Node) error {
		value, ok := one.(T)
		if !ok {
			return fmt.Errorf("%s: %w %s",
				name,
				ErrExpectedNumber,
				Node2String(one))
		}
		result = _f(result, value)
		return nil
	})
	return result, err
}

func CmdPlus(this Node) (Node, error) {
	cons, ok := this.(*Cons)
	if !ok {
		return nil, fmt.Errorf("+: %w", ErrExpectedCons)
	}
	if _, ok := cons.Car.(NodeInteger); ok {
		return inject(this, "+", func(left, right NodeInteger) NodeInteger {
			return left + right
		})
	}
	if _, ok := cons.Car.(NodeString); ok {
		return inject(this, "+", func(left, right NodeString) NodeString {
			return left + right
		})
	}
	return nil, fmt.Errorf("+: %w", ErrNotSupportType)
}

func CmdMinus(this Node) (Node, error) {
	return inject(this, "-", func(left, right NodeInteger) NodeInteger {
		return left - right
	})
}

func CmdMulti(this Node) (Node, error) {
	return inject(this, "*", func(left, right NodeInteger) NodeInteger {
		return left * right
	})
}

func CmdDevide(this Node) (Node, error) {
	return inject(this, "/", func(left, right NodeInteger) NodeInteger {
		return left / right
	})
}
