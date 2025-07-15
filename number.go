package gmnlisp

import (
	"context"
	"fmt"
	"math"
	"math/big"
)

var numberClass = &BuiltInClass{
	name: NewSymbol("<number>"),
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
		ObjectClass,
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
	if _n, ok := n.(BigInt); ok {
		return _n.Multi(ctx, w, i)
	} else if _n, ok := n.(Float); ok {
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
	return fmt.Sprintf("arithmetic error: %#v %#v %#v",
		e.Class.String(),
		e.Operation.String(),
		e.Operands.String())
}

func (e *ArithmeticError) Error() string {
	return e.String()
}

var divisionByZeroClass = registerNewAbstractClass[*ArithmeticError]("<division-by-zero>")

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

func funIsqrt(ctx context.Context, w *World, arg Node) (Node, error) {
	z := new(big.Float)
	if b, ok := arg.(BigInt); ok {
		if b.Int.Sign() < 0 {
			return callHandler[Integer](ctx, w, false, &DomainError{
				Object:        arg,
				ExpectedClass: floatClass,
			})
		}
		z = z.SetInt(b.Int)
	} else {
		i, err := ExpectClass[Integer](ctx, w, arg)
		if err != nil {
			return nil, err
		}
		if i < 0 {
			return callHandler[Integer](ctx, w, false, &DomainError{
				Object:        arg,
				ExpectedClass: floatClass,
			})
		}
		z = z.SetInt64(int64(i))
	}
	z = new(big.Float).Sqrt(z)
	v, _ := z.Int64()
	return Integer(v), nil
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

func (b BigInt) Multi(ctx context.Context, w *World, other Node) (Node, error) {
	var o *big.Int

	if i, ok := other.(Integer); ok {
		o = big.NewInt(int64(i))
	} else if b2, ok := other.(BigInt); ok {
		o = b2.Int
	} else {
		return nil, &DomainError{
			Reason: "not a integer or bigint",
			Object: other,
		}
	}
	return BigInt{Int: new(big.Int).Mul(b.Int, o)}, nil
}

func funReciprocal(ctx context.Context, w *World, x Node) (Node, error) {
	var f Float
	var err error
	if i, ok := x.(Integer); ok {
		if i == 0 {
			return callHandler[Node](ctx, w, true, &ArithmeticError{
				Operation: FunctionRef{value: Function1(funReciprocal)},
				Operands:  x,
				Class:     divisionByZeroClass,
			})
		}
		if 1%i == 0 {
			return Integer(1 / i), nil
		}
		f = Float(i)
	} else if f, err = ExpectClass[Float](ctx, w, x); err != nil {
		return nil, err
	} else if f == 0.0 {
		return callHandler[Node](ctx, w, true, &ArithmeticError{
			Operation: FunctionRef{value: Function1(funReciprocal)},
			Operands:  x,
			Class:     divisionByZeroClass,
		})
	}
	return 1 / f, nil
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

func funQuotient(ctx context.Context, w *World, args []Node) (Node, error) {
	resultIsFloat := false
	for _, arg1 := range args {
		if _, ok := arg1.(Integer); ok {
			continue
		}
		if _, ok := arg1.(BigInt); ok {
			continue
		}
		_, err := ExpectClass[Float](ctx, w, arg1)
		if err != nil {
			return nil, err
		}
		resultIsFloat = true
	}
	L := toBigFloat(args[0])
	if L == nil {
		panic("funQuotient(L)")
	}
	for divisor := args[1:]; len(divisor) > 0; divisor = divisor[1:] {
		R := toBigFloat(divisor[0])
		if R == nil {
			panic("funQuotient(R)")
		}
		if R.Sign() == 0 {
			return callHandler[Node](ctx, w, true, &ArithmeticError{
				Operation: FunctionRef{value: &Function{Min: 2, F: funQuotient}},
				Operands:  divisor[0],
				Class:     divisionByZeroClass,
			})
		}
		L = new(big.Float).Quo(L, R)
	}
	if resultIsFloat {
		f, _ := L.Float64()
		return Float(f), nil
	}
	return fromBigFloat(L), nil
}

func funFloat(ctx context.Context, w *World, x Node) (Node, error) {
	if _, ok := x.(Float); ok {
		return x, nil
	}
	if i, ok := x.(Integer); ok {
		return Float(float32(i)), nil
	}
	if b, ok := x.(BigInt); ok {
		f := new(big.Float).SetInt(b.Int)
		v, _ := f.Float64()
		return Float(v), nil
	}
	return callHandler[Node](ctx, w, true, &DomainError{
		Object:        x,
		ExpectedClass: floatClass,
	})
}
