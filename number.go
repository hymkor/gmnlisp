package gmnlisp

import (
	"fmt"
	"io"
)

type Integer int64

func (n Integer) PrintTo(w io.Writer) {
	fmt.Fprintf(w, "%d", int64(n))
}

func (n Integer) PrincTo(w io.Writer) {
	fmt.Fprintf(w, "%d", int64(n))
}

func (this Integer) IsNull() bool {
	return false
}

func (this Integer) Eval(*Instance) (Node, error) {
	return this, nil // errors.New("Integer can not be evaluate.")
}

func (this Integer) Equals(n Node) bool {
	ni, ok := n.(Integer)
	return ok && this == ni
}

func (this Integer) Plus(n Node) (Node, error) {
	if value, ok := n.(Integer); ok {
		return this + value, nil
	}
	if value, ok := n.(Float); ok {
		return Float(this) + value, nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n))
}

func (this Integer) Minus(n Node) (Node, error) {
	if value, ok := n.(Integer); ok {
		return this - value, nil
	}
	if value, ok := n.(Float); ok {
		return Float(this) - value, nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n))
}

func (this Integer) Multi(n Node) (Node, error) {
	if value, ok := n.(Integer); ok {
		return this * value, nil
	}
	if value, ok := n.(Float); ok {
		return Float(this) * value, nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n))
}

func (this Integer) Devide(n Node) (Node, error) {
	if value, ok := n.(Integer); ok {
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

func (this Integer) LessThan(n Node) (Node, error) {
	if value, ok := n.(Integer); ok {
		if this < value {
			return True, nil
		}
		return Null, nil
	}
	if value, ok := n.(Float); ok {
		if Float(this) < value {
			return True, nil
		}
		return Null, nil
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

func (Float) IsNull() bool {
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
	if value, ok := n.(Integer); ok {
		return this + Float(value), nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n))
}

func (this Float) Minus(n Node) (Node, error) {
	if value, ok := n.(Float); ok {
		return this - value, nil
	}
	if value, ok := n.(Integer); ok {
		return this - Float(value), nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n))
}

func (this Float) Multi(n Node) (Node, error) {
	if value, ok := n.(Float); ok {
		return this * value, nil
	}
	if value, ok := n.(Integer); ok {
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
	if value, ok := n.(Integer); ok {
		if value == 0 {
			return nil, ErrDevisionByZero
		}
		return this / Float(value), nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n))
}

func (this Float) LessThan(n Node) (Node, error) {
	if value, ok := n.(Float); ok {
		if this < value {
			return True, nil
		}
		return Null, nil
	}
	if value, ok := n.(Integer); ok {
		if this < Float(value) {
			return True, nil
		}
		return Null, nil
	}
	return nil, fmt.Errorf("%w: `%s`", ErrNotSupportType, toString(n))
}
