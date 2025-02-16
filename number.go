package gmnlisp

import (
	"context"
	"fmt"
	"math"
	"math/big"
)

var numberClass = &_BuiltInClass{
	name: NewSymbol("number"),
	instanceP: func(n Node) bool {
		if _, ok := n.(Integer); ok {
			return true
		}
		if _, ok := n.(Float); ok {
			return true
		}
		return false
	},
	create: func() Node {
		return nil
	},
	super: []Class{
		objectClass,
	},
}

type Integer int64

var integerClass = registerNewBuiltInClass[Integer]("<integer>", numberClass)

func (i Integer) ClassOf() Class {
	return integerClass
}

func (i Integer) String() string {
	return fmt.Sprintf("%d", int64(i))
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
	if _n, ok := n.(Float); ok {
		return Float(i) + _n, nil
	}
	_n, err := ExpectClass[Integer](ctx, w, n)
	if err == nil {
		return i + _n, nil
	}
	return nil, err
}

func (i Integer) Sub(ctx context.Context, w *World, n Node) (Node, error) {
	if _n, ok := n.(Float); ok {
		return Float(i) - _n, nil
	}
	_n, err := ExpectClass[Integer](ctx, w, n)
	if err == nil {
		return i - _n, nil
	}
	return nil, err
}

func (i Integer) Multi(ctx context.Context, w *World, n Node) (Node, error) {
	if _n, ok := n.(Float); ok {
		return Float(i) * _n, nil
	}
	_n, err := ExpectClass[Integer](ctx, w, n)
	if err == nil {
		return i * _n, nil
	}
	return nil, err
}

type ArithmeticError struct {
	Operation FunctionRef
	Operands  Node
	Class     Class
}

var arithmeticErrorClass = registerNewBuiltInClass[*ArithmeticError]("<arithmetic-error-class>")

func (e *ArithmeticError) ClassOf() Class {
	if e != nil && e.Class != nil {
		return e.Class
	}
	return arithmeticErrorClass
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

func (e *ArithmeticError) String() string {
	return fmt.Sprintf("%s: (%#v %#v)",
		e.Class.String(),
		e.Operation.String(),
		e.Operands.String())
}

func (e *ArithmeticError) Error() string {
	return e.String()
}

var divisionByZeroClass = registerNewAbstractClass[*ArithmeticError]("<division-by-zero>")

func raiseDivisionByZero(ctx context.Context, w *World, m, n Node) (Node, error) {
	return callHandler[Node](ctx, w, true, &ArithmeticError{
		Operation: FunctionRef{value: &Function{C: 2, F: funDevide}},
		Operands:  List(m, n),
		Class:     divisionByZeroClass,
	})
}

func funArithmeticErrorOperation(ctx context.Context, w *World, n Node) (Node, error) {
	e, err := ExpectClass[*ArithmeticError](ctx, w, n)
	if err != nil {
		return nil, err
	}
	return e.Operation, nil
}

func funArithmeticErrorOperands(ctx context.Context, w *World, n Node) (Node, error) {
	e, err := ExpectClass[*ArithmeticError](ctx, w, n)
	if err != nil {
		return nil, err
	}
	return e.Operands, nil
}

func (i Integer) LessThan(ctx context.Context, w *World, n Node) (bool, error) {
	if _n, ok := n.(Float); ok {
		return Float(i) < _n, nil
	}
	_n, err := ExpectClass[Integer](ctx, w, n)
	if err == nil {
		return i < _n, nil
	}
	return false, err
}

type Float float64

var floatClass = registerNewBuiltInClass[Float]("<float>")

func (Float) ClassOf() Class {
	return floatClass
}

func (f Float) String() string {
	return fmt.Sprintf("%f", float64(f))
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

func funSqrt(ctx context.Context, w *World, arg Node) (Node, error) {
	cast := func(f float64) Node {
		return Float(f)
	}
	var f float64
	if n, ok := arg.(Integer); ok {
		f = float64(int(n))
		cast = func(f float64) Node {
			v := int(f)
			if v*v == int(n) {
				return Integer(v)
			}
			return Float(f)
		}
	} else if _f, err := ExpectClass[Float](ctx, w, arg); err != nil {
		return nil, err
	} else {
		f = float64(_f)
	}
	if f < 0 {
		return callHandler[Node](ctx, w, true, &DomainError{
			Object:        arg,
			ExpectedClass: floatClass,
		})
	}
	return cast(math.Sqrt(f)), nil
}

type BigInt struct {
	*big.Int
}

func (b BigInt) Equals(n Node, m EqlMode) bool {
	if bi, ok := n.(BigInt); ok {
		return b.Int.Cmp(bi.Int) == 0
	}
	if _n, ok := n.(Integer); ok {
		bi := big.NewInt(int64(_n))
		return b.Int.Cmp(bi) == 0
	}
	return false
}

func (b BigInt) ClassOf() Class {
	return integerClass
}
