package gmnlisp

import (
	"context"
	"math"
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

func funLog(ctx context.Context, w *World, x Node) (Node, error) {
	var f float64
	if i, ok := x.(Integer); ok {
		f = float64(int(i))
	} else if _f, err := ExpectClass[Float](ctx, w, x); err != nil {
		return nil, err
	} else {
		f = float64(_f)
	}
	if f <= 0 {
		return callHandler[Node](ctx, w, true, &DomainError{
			Object:        x,
			ExpectedClass: floatClass,
		})
	}
	return Float(math.Log(f)), nil
}
