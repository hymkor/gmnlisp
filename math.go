package gmnlisp

import (
	"context"
)

func funMath1(fn func(n float64) float64) SpecialF {
	return func(ctx context.Context, w *World, node Node) (Node, error) {
		value, node, err := w.ShiftAndEvalCar(ctx, node)
		if err != nil {
			return nil, err
		}
		if IsSome(node) {
			return nil, ErrTooManyArguments
		}
		if i, ok := value.(Integer); ok {
			return Float(fn(float64(int(i)))), nil
		}
		f, err := ExpectClass[Float](ctx, w, value)
		if err != nil {
			return nil, err
		}
		return Float(fn(float64(f))), nil
	}
}
