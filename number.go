package gmnlisp

import (
	"fmt"
	"io"
)

type NodeInteger int64

func (n NodeInteger) PrintTo(w io.Writer) {
	fmt.Fprintf(w, "%d", int64(n))
}

func (n NodeInteger) PrincTo(w io.Writer) {
	fmt.Fprintf(w, "%d", int64(n))
}

func (this NodeInteger) Null() bool {
	return false
}

func (this NodeInteger) Eval(*Instance) (Node, error) {
	return this, nil // errors.New("Integer can not be evaluate.")
}

func (this NodeInteger) Equals(n Node) bool {
	ni, ok := n.(NodeInteger)
	return ok && this == ni
}

func (this NodeInteger) Plus(n Node) (Node, error) {
	if value, ok := n.(NodeInteger); ok {
		return this + value, nil
	}
	if value, ok := n.(Float); ok {
		return Float(this) + value, nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n))
}

func (this NodeInteger) Minus(n Node) (Node, error) {
	if value, ok := n.(NodeInteger); ok {
		return this - value, nil
	}
	if value, ok := n.(Float); ok {
		return Float(this) - value, nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n))
}

func (this NodeInteger) Multi(n Node) (Node, error) {
	if value, ok := n.(NodeInteger); ok {
		return this * value, nil
	}
	if value, ok := n.(Float); ok {
		return Float(this) * value, nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n))
}

func (this NodeInteger) Devide(n Node) (Node, error) {
	if value, ok := n.(NodeInteger); ok {
		if value == 0 {
			return nil, ErrDevisionByZero
		}
		return this / value, nil
	}
	if value, ok := n.(Float); ok {
		if value == 0 {
			return nil, ErrDevisionByZero
		}
		return Float(this) / value, nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n))
}

type Float float64

func (n Float) PrintTo(w io.Writer) {
	fmt.Fprintf(w, "%f", float64(n))
}

func (n Float) PrincTo(w io.Writer) {
	fmt.Fprintf(w, "%f", float64(n))
}

func (Float) Null() bool {
	return false
}

func (nf Float) Eval(*Instance) (Node, error) {
	return nf, nil
}

func (nf Float) Equals(n Node) bool {
	v, ok := n.(Float)
	return ok && nf == v
}

func (this Float) Plus(n Node) (Node, error) {
	if value, ok := n.(Float); ok {
		return this + value, nil
	}
	if value, ok := n.(NodeInteger); ok {
		return this + Float(value), nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n))
}

func (this Float) Minus(n Node) (Node, error) {
	if value, ok := n.(Float); ok {
		return this - value, nil
	}
	if value, ok := n.(NodeInteger); ok {
		return this - Float(value), nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n))
}

func (this Float) Multi(n Node) (Node, error) {
	if value, ok := n.(Float); ok {
		return this * value, nil
	}
	if value, ok := n.(NodeInteger); ok {
		return this * Float(value), nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n))
}

func (this Float) Devide(n Node) (Node, error) {
	if value, ok := n.(Float); ok {
		if value == 0 {
			return nil, ErrDevisionByZero
		}
		return this / value, nil
	}
	if value, ok := n.(NodeInteger); ok {
		if value == 0 {
			return nil, ErrDevisionByZero
		}
		return this / Float(value), nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n))
}
