package common

import (
	"context"
	. "github.com/hymkor/gmnlisp"
)

type canPlus interface {
	Node
	Add(Node) (Node, error)
}

type canMinus interface {
	Node
	Sub(Node) (Node, error)
}

func incfDecf(ctx context.Context, w *World, list Node, f func(Symbol, Node, Node) (Node, error)) (Node, error) {
	var name Node
	var err error

	name, list, err = Shift(list)
	if err != nil {
		return nil, err
	}
	var right Node = Integer(1)
	if HasValue(list) {
		right, list, err = w.ShiftAndEvalCar(ctx, list)
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
