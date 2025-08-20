package gmnlisp

import (
	"context"
	"fmt"
	"math"
	"math/big"
)

type Float float64

var floatClass = registerNewBuiltInClass[Float]("<float>")

func (Float) ClassOf() Class {
	return floatClass
}

func (f Float) String() string {
	return fmt.Sprintf("%f", float64(f))
}

func floatEqualsRelative(a, b float64, relEps float64) bool {
	diff := math.Abs(a - b)
	norm := math.Max(math.Abs(a), math.Abs(b))
	return diff <= relEps*norm
}

func (f Float) Equals(n Node, m EqlMode) bool {
	if m == EQUALP {
		if _n, ok := n.(Float); ok && f == _n {
			return true
		}
		_n, ok := n.(Integer)
		return ok && f == Float(_n)
	} else {
		v, ok := n.(Float)
		return ok && floatEqualsRelative(float64(f), float64(v), 1e-9)
	}
}

func (f Float) Add(ctx context.Context, w *World, n Node) (Node, error) {
	if _n, ok := n.(Integer); ok {
		return f + Float(_n), nil
	}
	_n, err := ExpectClass[Float](ctx, w, n)
	if err == nil {
		return f + _n, nil
	}
	return nil, err
}

func (f Float) Sub(ctx context.Context, w *World, n Node) (Node, error) {
	if _n, ok := n.(Integer); ok {
		return f - Float(_n), nil
	}
	_n, err := ExpectClass[Float](ctx, w, n)
	if err == nil {
		return f - _n, nil
	}
	return nil, err
}

func (f Float) Multi(ctx context.Context, w *World, n Node) (Node, error) {
	if _n, ok := n.(Integer); ok {
		return f * Float(_n), nil
	}
	_n, err := ExpectClass[Float](ctx, w, n)
	if err == nil {
		return f * _n, nil
	}
	return nil, err
}

func (f Float) LessThan(ctx context.Context, w *World, n Node) (bool, error) {
	if _n, ok := n.(Integer); ok {
		return f < Float(_n), nil
	}
	_n, err := ExpectClass[Float](ctx, w, n)
	if err == nil {
		return f < _n, nil
	}
	return false, err
}

func toBigFloat(v Node) *big.Float {
	if i, ok := v.(Integer); ok {
		return big.NewFloat(float64(int64(i)))
	}
	if f, ok := v.(Float); ok {
		return big.NewFloat(float64(f))
	}
	if b, ok := v.(BigInt); ok {
		return new(big.Float).SetInt(b.Int)
	}
	return nil
}

func fromBigFloat(v *big.Float) Node {
	if v.IsInt() {
		i, _ := v.Int(nil)
		if i.IsInt64() {
			return Integer(i.Int64())
		}
		return BigInt{Int: i}
	}
	r, _ := v.Float64()
	return Float(r)
}
