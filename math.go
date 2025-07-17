package gmnlisp

import (
	"context"
	"errors"
	"math"
	"math/big"
)

func checkFiniteFloat(v float64) (Node, error) {
	if math.IsNaN(v) {
		return nil, errors.New("NaN")
	}
	if math.IsInf(v, 0) {
		return nil, errors.New("Inf")
	}
	return Float(v), nil
}

func funMath1(fn func(n float64) float64) SpecialF {
	return func(ctx context.Context, w *World, node Node) (Node, error) {
		value, node, err := w.ShiftAndEvalCar(ctx, node)
		if err != nil {
			return nil, err
		}
		if IsSome(node) {
			return nil, ErrTooManyArguments
		}
		var result float64
		if i, ok := value.(Integer); ok {
			result = fn(float64(int(i)))
		} else {
			f, err := ExpectClass[Float](ctx, w, value)
			if err != nil {
				return nil, err
			}
			result = fn(float64(f))
		}
		return checkFiniteFloat(result)
	}
}

func toFloat64(ctx context.Context, w *World, x Node, hasPeriod *bool) (float64, error) {
	if i, ok := x.(Integer); ok {
		return float64(int(i)), nil
	}
	f, err := ExpectClass[Float](ctx, w, x)
	if err != nil {
		return 0.0, err
	}
	if hasPeriod != nil {
		*hasPeriod = true
	}
	return float64(f), nil
}

func funAtan2(ctx context.Context, w *World, x, y Node) (Node, error) {
	X, err := toFloat64(ctx, w, x, nil)
	if err != nil {
		return nil, err
	}
	Y, err := toFloat64(ctx, w, y, nil)
	if err != nil {
		return nil, err
	}
	return Float(math.Atan2(X, Y)), nil
}

func funLog(ctx context.Context, w *World, x Node) (Node, error) {
	f, err := toFloat64(ctx, w, x, nil)
	if err != nil {
		return nil, err
	}
	if f <= 0 {
		return callHandler[Node](ctx, w, true, &DomainError{
			Object:        x,
			ExpectedClass: floatClass,
		})
	}
	return Float(math.Log(f)), nil
}

func funExpt(ctx context.Context, w *World, x1, x2 Node) (Node, error) {
	hasPeriod := false
	f1, err := toFloat64(ctx, w, x1, &hasPeriod)
	if err != nil {
		return nil, err
	}
	f2, err := toFloat64(ctx, w, x2, &hasPeriod)
	if err != nil {
		return nil, err
	}
	if (x1 == Integer(0) || x1 == Float(0)) && (f2 < 0 || x2 == Float(0)) {
		return callHandler[*ArithmeticError](ctx, w, true, &ArithmeticError{
			Operation: FunctionRef{value: Function2(funExpt)},
			Operands:  List(x1, x2),
			Class:     arithmeticErrorClass,
		})
	}
	if _, ok := x2.(Integer); !ok && f1 < 0 {
		return callHandler[*ArithmeticError](ctx, w, true, &ArithmeticError{
			Operation: FunctionRef{value: Function2(funExpt)},
			Operands:  List(x1, x2),
			Class:     arithmeticErrorClass,
		})
	}
	result := math.Pow(f1, f2)
	if hasPeriod || f2 < 0 {
		if math.IsInf(result, 0) {
			return callHandler[*ArithmeticError](ctx, w, true, &ArithmeticError{
				Operation: FunctionRef{value: Function2(funExpt)},
				Operands:  List(x1, x2),
				Class:     floatingPointOverflowClass,
			})
		}
		if math.IsNaN(result) {
			return callHandler[*ArithmeticError](ctx, w, true, &ArithmeticError{
				Operation: FunctionRef{value: Function2(funExpt)},
				Operands:  List(x1, x2),
				Class:     arithmeticErrorClass,
			})
		}
		return Float(result), nil
	}
	if math.MinInt64 < result && result < math.MaxInt64 {
		return Integer(int64(result)), nil
	}
	i, _ := big.NewFloat(result).Int(nil)
	return BigInt{Int: i}, nil
}
