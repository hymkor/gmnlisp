package gmnlisp

import (
	"context"
	"fmt"
	"io"
	"math"
)

type Integer int64

var integerClass = registerNewBuiltInClass[Integer]("<integer>")

func (i Integer) ClassOf() Class {
	return integerClass
}

func (i Integer) PrintTo(w io.Writer, m PrintMode) (int, error) {
	return fmt.Fprintf(w, "%d", int64(i))
}

func (i Integer) Eval(context.Context, *World) (Node, error) {
	return i, nil
}

func (i Integer) Equals(n Node, m EqlMode) bool {
	if m == EQUALP {
		if _n, ok := n.(Integer); ok && i == _n {
			return true
		}
		_n, ok := n.(Float)
		return ok && Float(i) == _n
	} else {
		_n, ok := n.(Integer)
		return ok && i == _n
	}
}

func (i Integer) Add(ctx context.Context, w *World, n Node) (Node, error) {
	_n, err := ExpectClass[Integer](ctx, w, n)
	if err == nil {
		return i + _n, nil
	}
	if _n, ok := n.(Float); ok {
		return Float(i) + _n, nil
	}
	return nil, err
}

func (i Integer) Sub(n Node) (Node, error) {
	_n, err := ExpectInteger(n)
	if err == nil {
		return i - _n, nil
	}
	if _n, ok := n.(Float); ok {
		return Float(i) - _n, nil
	}
	return nil, err
}

func (i Integer) Multi(n Node) (Node, error) {
	_n, err := ExpectInteger(n)
	if err == nil {
		return i * _n, nil
	}
	if _n, ok := n.(Float); ok {
		return Float(i) * _n, nil
	}
	return nil, err
}

func (i Integer) Divide(n Node) (Node, error) {
	_n, err := ExpectInteger(n)
	if err == nil {
		if _n == 0 {
			return nil, ErrDevisionByZero
		}
		return i / _n, nil
	}
	if _n, ok := n.(Float); ok {
		if _n == 0 {
			return nil, ErrDevisionByZero
		}
		return Float(i) / _n, nil
	}
	return nil, err
}

func (i Integer) LessThan(ctx context.Context, w *World, n Node) (bool, error) {
	_n, err := ExpectClass[Integer](ctx, w, n)
	if err == nil {
		return i < _n, nil
	}
	if _n, ok := n.(Float); ok {
		return Float(i) < _n, nil
	}
	return false, err
}

type Float float64

var floatClass = registerNewBuiltInClass[Float]("<float>")

func (Float) ClassOf() Class {
	return floatClass
}

func (f Float) PrintTo(w io.Writer, m PrintMode) (int, error) {
	return fmt.Fprintf(w, "%f", float64(f))
}

func (f Float) Eval(context.Context, *World) (Node, error) {
	return f, nil
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
		return ok && f == v
	}
}

func (f Float) Add(ctx context.Context, w *World, n Node) (Node, error) {
	_n, err := ExpectClass[Float](ctx, w, n)
	if err == nil {
		return f + _n, nil
	}
	if _n, ok := n.(Integer); ok {
		return f + Float(_n), nil
	}
	return nil, err
}

func (f Float) Sub(n Node) (Node, error) {
	_n, err := ExpectFloat(n)
	if err == nil {
		return f - _n, nil
	}
	if _n, ok := n.(Integer); ok {
		return f - Float(_n), nil
	}
	return nil, err
}

func (f Float) Multi(n Node) (Node, error) {
	_n, err := ExpectFloat(n)
	if err == nil {
		return f * _n, nil
	}
	if _n, ok := n.(Integer); ok {
		return f * Float(_n), nil
	}
	return nil, err
}

func (f Float) Divide(n Node) (Node, error) {
	_n, err := ExpectFloat(n)
	if err == nil {
		if _n == 0 {
			return nil, ErrDevisionByZero
		}
		return f / _n, nil
	}
	if _n, ok := n.(Integer); ok {
		if _n == 0 {
			return nil, ErrDevisionByZero
		}
		return f / Float(_n), nil
	}
	return nil, err
}

func (f Float) LessThan(ctx context.Context, w *World, n Node) (bool, error) {
	_n, err := ExpectClass[Float](ctx, w, n)
	if err == nil {
		return f < _n, nil
	}
	if _n, ok := n.(Integer); ok {
		return f < Float(_n), nil
	}
	return false, err
}

func funSqrt(ctx context.Context, w *World, node []Node) (Node, error) {
	n, err := ExpectFloat(node[0])
	if err != nil {
		return nil, err
	}
	v := math.Sqrt(float64(n))
	return Float(v), nil
}
