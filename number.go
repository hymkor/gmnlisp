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
		_, ok := n.(BigInt)
		return ok
	},
	create: func() Node { return nil },
	super:  []Class{ObjectClass},
}

type ArithmeticError struct {
	Operation FunctionRef
	Operands  Node
}

type ArithmeticErrorInterface interface {
	GetOperation() FunctionRef
	GetOperands() Node
	Node
}

func (A *ArithmeticError) GetOperation() FunctionRef {
	return A.Operation
}

func (A *ArithmeticError) GetOperands() Node {
	return A.Operands
}

var arithmeticErrorClass = registerClass(&BuiltInClass{
	name: NewSymbol("<arithmetic-error>"),
	instanceP: func(v Node) bool {
		_, ok := v.(ArithmeticErrorInterface)
		return ok
	},
	create: func() Node { return nil },
	super:  []Class{ObjectClass, seriousConditionClass, errorClass},
})

func (e *ArithmeticError) ClassOf() Class {
	return arithmeticErrorClass
}

func (e *ArithmeticError) Equals(other Node, mode EqlMode) bool {
	o, ok := other.(*ArithmeticError)
	if !ok {
		return false
	}
	return e.Operands.Equals(o.Operands, mode) &&
		e.Operation.Equals(o.Operation, mode)
}

func (e *ArithmeticError) String() string {
	return fmt.Sprintf("arithmetic error: %#v %#v",
		e.Operation.String(),
		e.Operands.String())
}

func (e *ArithmeticError) Error() string {
	return e.String()
}

var divisionByZeroClass = registerNewAbstractClass[*DivisionByZero]("<division-by-zero>", ObjectClass, seriousConditionClass, errorClass, arithmeticErrorClass)

func funArithmeticErrorOperation(ctx context.Context, w *World, n Node) (Node, error) {
	e, err := ExpectInterface[ArithmeticErrorInterface](ctx, w, n, arithmeticErrorClass)
	if err != nil {
		return nil, err
	}
	return e.GetOperation(), nil
}

func funArithmeticErrorOperands(ctx context.Context, w *World, n Node) (Node, error) {
	e, err := ExpectInterface[ArithmeticErrorInterface](ctx, w, n, arithmeticErrorClass)
	if err != nil {
		return nil, err
	}
	return e.GetOperands(), nil
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

func promoteOperands(
	ctx context.Context,
	w *World,
	a, b Node,
	doInt func(Integer, Integer) (Node, error),
	doBigInt func(BigInt, BigInt) (Node, error),
	doFloat func(Float, Float) (Node, error),
	doBigFlt func(*big.Float, *big.Float) (Node, error)) (Node, error) {

	if A, ok := a.(Float); ok {
		if B, ok := b.(Float); ok {
			return doFloat(A, B)
		}
		if B, ok := b.(Integer); ok {
			return doFloat(A, Float(float64(int64(B))))
		}
		if B, ok := b.(BigInt); ok {
			aa := big.NewFloat(float64(A))
			bb := new(big.Float).SetInt(B.Int)
			return doBigFlt(aa, bb)
		}
	} else if A, ok := a.(BigInt); ok {
		if B, ok := b.(BigInt); ok {
			return doBigInt(A, B)
		}
		if B, ok := b.(Integer); ok {
			return doBigInt(A, BigInt{Int: big.NewInt(int64(B))})
		}
		if B, ok := b.(Float); ok {
			aa := new(big.Float).SetInt(A.Int)
			bb := big.NewFloat(float64(B))
			return doBigFlt(aa, bb)
		}
	} else if A, ok := a.(Integer); ok {
		if B, ok := b.(BigInt); ok {
			return doBigInt(BigInt{Int: big.NewInt(int64(A))}, B)
		}
		if B, ok := b.(Integer); ok {
			return doInt(A, B)
		}
		if B, ok := b.(Float); ok {
			return doFloat(Float(float64(int64(A))), Float(float64(B)))
		}
	} else {
		return callHandler[Node](ctx, w, false, &DomainError{
			Reason: "not a integer, bigint and float",
			Object: a,
		})
	}
	return callHandler[Node](ctx, w, false, &DomainError{
		Reason: "not a integer, bigint and float",
		Object: b,
	})
}

type DivisionByZero struct {
	ArithmeticError
}

func (d *DivisionByZero) ClassOf() Class {
	return divisionByZeroClass
}

func (d *DivisionByZero) Equals(other Node, m EqlMode) bool {
	if o, ok := other.(*DivisionByZero); ok {
		return d.ArithmeticError.Equals(&o.ArithmeticError, m)
	}
	return false
}

var _ ArithmeticErrorInterface = &DivisionByZero{}

func funReciprocal(ctx context.Context, w *World, x Node) (Node, error) {
	var f Float
	var err error
	if i, ok := x.(Integer); ok {
		if i == 0 {
			return callHandler[Node](ctx, w, true, &DivisionByZero{
				ArithmeticError: ArithmeticError{
					Operation: FunctionRef{value: Function1(funReciprocal)},
					Operands:  x,
				},
			})
		}
		if 1%i == 0 {
			return Integer(1 / i), nil
		}
		f = Float(i)
	} else if f, err = ExpectClass[Float](ctx, w, x); err != nil {
		return nil, err
	} else if f == 0.0 {
		return callHandler[Node](ctx, w, true, &DivisionByZero{
			ArithmeticError: ArithmeticError{
				Operation: FunctionRef{value: Function1(funReciprocal)},
				Operands:  x,
			},
		})
	}
	return 1 / f, nil
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
			return callHandler[Node](ctx, w, true, &DivisionByZero{
				ArithmeticError: ArithmeticError{
					Operation: FunctionRef{value: &Function{Min: 2, F: funQuotient}},
					Operands:  divisor[0],
				},
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
