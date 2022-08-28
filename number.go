package gmnlisp

import (
	"context"
	"fmt"
	"io"
)

type Integer int64

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

func (i Integer) Add(n Node) (Node, error) {
	if _n, ok := n.(Integer); ok {
		return i + _n, nil
	}
	if _n, ok := n.(Float); ok {
		return Float(i) + _n, nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n, PRINT))
}

func (i Integer) Sub(n Node) (Node, error) {
	if _n, ok := n.(Integer); ok {
		return i - _n, nil
	}
	if _n, ok := n.(Float); ok {
		return Float(i) - _n, nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n, PRINT))
}

func (i Integer) Multi(n Node) (Node, error) {
	if _n, ok := n.(Integer); ok {
		return i * _n, nil
	}
	if _n, ok := n.(Float); ok {
		return Float(i) * _n, nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n, PRINT))
}

func (i Integer) Divide(n Node) (Node, error) {
	if _n, ok := n.(Integer); ok {
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
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n, PRINT))
}

func (i Integer) LessThan(n Node) (bool, error) {
	if _n, ok := n.(Integer); ok {
		return i < _n, nil
	}
	if _n, ok := n.(Float); ok {
		return Float(i) < _n, nil
	}
	return false, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n, PRINT))
}

type Float float64

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

func (f Float) Add(n Node) (Node, error) {
	if _n, ok := n.(Float); ok {
		return f + _n, nil
	}
	if _n, ok := n.(Integer); ok {
		return f + Float(_n), nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n, PRINT))
}

func (f Float) Sub(n Node) (Node, error) {
	if _n, ok := n.(Float); ok {
		return f - _n, nil
	}
	if _n, ok := n.(Integer); ok {
		return f - Float(_n), nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n, PRINT))
}

func (f Float) Multi(n Node) (Node, error) {
	if _n, ok := n.(Float); ok {
		return f * _n, nil
	}
	if _n, ok := n.(Integer); ok {
		return f * Float(_n), nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n, PRINT))
}

func (f Float) Divide(n Node) (Node, error) {
	if _n, ok := n.(Float); ok {
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
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n, PRINT))
}

func (f Float) LessThan(n Node) (bool, error) {
	if _n, ok := n.(Float); ok {
		return f < _n, nil
	}
	if _n, ok := n.(Integer); ok {
		return f < Float(_n), nil
	}
	return false, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n, PRINT))
}
