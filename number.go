package gmnlisp

import (
	"context"
	"fmt"
	"io"
	"math"
	"strings"
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

func (i Integer) Sub(ctx context.Context, w *World, n Node) (Node, error) {
	_n, err := ExpectClass[Integer](ctx, w, n)
	if err == nil {
		return i - _n, nil
	}
	if _n, ok := n.(Float); ok {
		return Float(i) - _n, nil
	}
	return nil, err
}

func (i Integer) Multi(ctx context.Context, w *World, n Node) (Node, error) {
	_n, err := ExpectClass[Integer](ctx, w, n)
	if err == nil {
		return i * _n, nil
	}
	if _n, ok := n.(Float); ok {
		return Float(i) * _n, nil
	}
	return nil, err
}

type ArithmeticError struct {
	Operation FunctionRef
	Operands  Node
	Class     Class
}

func (e *ArithmeticError) ClassOf() Class {
	return e.Class
}

func (e *ArithmeticError) Eval(context.Context, *World) (Node, error) {
	return e, nil
}

func (e *ArithmeticError) Equals(other Node, mode EqlMode) bool {
	o, ok := other.(*ArithmeticError)
	if !ok {
		return false
	}
	return e.Operands.Equals(o.Operands, mode) &&
		e.Operation.Equals(o.Operation, mode) &&
		e.Class.Equals(o.Class, mode)
}

func (e *ArithmeticError) PrintTo(w io.Writer, mode PrintMode) (int, error) {
	return fmt.Fprintf(w, "%s: (%#v %#v)",
		e.Class.String(),
		e.Operation.String(),
		e.Operands.String())
}

func (e *ArithmeticError) String() string {
	var buffer strings.Builder
	e.PrintTo(&buffer, PRINC)
	return buffer.String()
}

func (e *ArithmeticError) GoString() string {
	return e.String()
}

func (e *ArithmeticError) Error() string {
	return e.String()
}

var divisionByZeroClass = registerNewBuiltInClass[*ArithmeticError]("<division-by-zero>")

func raiseDivisionByZero(ctx context.Context, w *World, m, n Node) (Node, error) {
	return callHandler[Node](ctx, w, true, &ArithmeticError{
		Operation: FunctionRef{value: &Function{F: funDevide}},
		Operands:  List(m, n),
		Class:     divisionByZeroClass,
	})
}

func funArithmeticErrorOperation(ctx context.Context, w *World, n []Node) (Node, error) {
	e, err := ExpectClass[*ArithmeticError](ctx, w, n[0])
	if err != nil {
		return nil, err
	}
	return e.Operation, nil
}

func funArithmeticErrorOperands(ctx context.Context, w *World, n []Node) (Node, error) {
	e, err := ExpectClass[*ArithmeticError](ctx, w, n[0])
	if err != nil {
		return nil, err
	}
	return e.Operands, nil
}

func (i Integer) Divide(ctx context.Context, w *World, n Node) (Node, error) {
	_n, err := ExpectClass[Integer](ctx, w, n)
	if err == nil {
		if _n == 0 {
			return raiseDivisionByZero(ctx, w, i, n)
		}
		return i / _n, nil
	}
	if _n, ok := n.(Float); ok {
		if _n == 0 {
			return raiseDivisionByZero(ctx, w, i, n)
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

func (f Float) Sub(ctx context.Context, w *World, n Node) (Node, error) {
	_n, err := ExpectClass[Float](ctx, w, n)
	if err == nil {
		return f - _n, nil
	}
	if _n, ok := n.(Integer); ok {
		return f - Float(_n), nil
	}
	return nil, err
}

func (f Float) Multi(ctx context.Context, w *World, n Node) (Node, error) {
	_n, err := ExpectClass[Float](ctx, w, n)
	if err == nil {
		return f * _n, nil
	}
	if _n, ok := n.(Integer); ok {
		return f * Float(_n), nil
	}
	return nil, err
}

func (f Float) Divide(ctx context.Context, w *World, n Node) (Node, error) {
	_n, err := ExpectClass[Float](ctx, w, n)
	if err == nil {
		if _n == 0 {
			return raiseDivisionByZero(ctx, w, f, n)
		}
		return f / _n, nil
	}
	if _n, ok := n.(Integer); ok {
		if _n == 0 {
			return raiseDivisionByZero(ctx, w, f, n)
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
	n, err := ExpectClass[Float](ctx, w, node[0])
	if err != nil {
		return nil, err
	}
	v := math.Sqrt(float64(n))
	return Float(v), nil
}
