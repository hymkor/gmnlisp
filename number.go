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
	if value, ok := n.(NodeFloat); ok {
		return NodeFloat(this) + value, nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n))
}

func (this NodeInteger) Minus(n Node) (Node, error) {
	if value, ok := n.(NodeInteger); ok {
		return this - value, nil
	}
	if value, ok := n.(NodeFloat); ok {
		return NodeFloat(this) - value, nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n))
}

func (this NodeInteger) Multi(n Node) (Node, error) {
	if value, ok := n.(NodeInteger); ok {
		return this * value, nil
	}
	if value, ok := n.(NodeFloat); ok {
		return NodeFloat(this) * value, nil
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
	if value, ok := n.(NodeFloat); ok {
		if value == 0 {
			return nil, ErrDevisionByZero
		}
		return NodeFloat(this) / value, nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n))
}

type NodeFloat float64

func (n NodeFloat) PrintTo(w io.Writer) {
	fmt.Fprintf(w, "%f", float64(n))
}

func (n NodeFloat) PrincTo(w io.Writer) {
	fmt.Fprintf(w, "%f", float64(n))
}

func (NodeFloat) Null() bool {
	return false
}

func (nf NodeFloat) Eval(*Instance) (Node, error) {
	return nf, nil
}

func (nf NodeFloat) Equals(n Node) bool {
	v, ok := n.(NodeFloat)
	return ok && nf == v
}

func (this NodeFloat) Plus(n Node) (Node, error) {
	if value, ok := n.(NodeFloat); ok {
		return this + value, nil
	}
	if value, ok := n.(NodeInteger); ok {
		return this + NodeFloat(value), nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n))
}

func (this NodeFloat) Minus(n Node) (Node, error) {
	if value, ok := n.(NodeFloat); ok {
		return this - value, nil
	}
	if value, ok := n.(NodeInteger); ok {
		return this - NodeFloat(value), nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n))
}

func (this NodeFloat) Multi(n Node) (Node, error) {
	if value, ok := n.(NodeFloat); ok {
		return this * value, nil
	}
	if value, ok := n.(NodeInteger); ok {
		return this * NodeFloat(value), nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n))
}

func (this NodeFloat) Devide(n Node) (Node, error) {
	if value, ok := n.(NodeFloat); ok {
		if value == 0 {
			return nil, ErrDevisionByZero
		}
		return this / value, nil
	}
	if value, ok := n.(NodeInteger); ok {
		if value == 0 {
			return nil, ErrDevisionByZero
		}
		return this / NodeFloat(value), nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n))
}
