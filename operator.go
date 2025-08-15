package gmnlisp

import (
	"context"
	"math"
	"math/big"
)

func notNullToTrue(v Node, err error) (Node, error) {
	if v == nil || err != nil {
		return Null, err
	}
	if _, ok := v.(nullType); ok {
		return Null, err
	}
	return True, err
}

type canPlus interface {
	Node
	Add(context.Context, *World, Node) (Node, error)
}

func funAdd(ctx context.Context, w *World, args []Node) (Node, error) {
	switch len(args) {
	case 0:
		return Integer(0), nil
	case 1:
		_, err := ExpectInterface[canPlus](ctx, w, args[0], floatClass)
		if err != nil {
			return nil, err
		}
	}
	return inject(args, func(left, right Node) (Node, error) {
		_left, err := ExpectInterface[canPlus](ctx, w, left, floatClass)
		if err != nil {
			return nil, err
		}
		return _left.Add(ctx, w, right)
	})
}

type canMinus interface {
	Node
	Sub(context.Context, *World, Node) (Node, error)
}

func funSub(ctx context.Context, w *World, args []Node) (Node, error) {
	for _, v := range args {
		if !numberClass.InstanceP(v) {
			return nil, &DomainError{
				Object:        v,
				ExpectedClass: numberClass,
			}
		}
	}
	if len(args) == 1 {
		class := args[0].ClassOf()
		zero := class.Create()
		z, err := ExpectInterface[canMinus](ctx, w, zero, class)
		if err != nil {
			return nil, err
		}
		return z.Sub(ctx, w, args[0])
	}
	return inject(args, func(left, right Node) (Node, error) {
		_left, err := ExpectInterface[canMinus](ctx, w, left, floatClass)
		if err != nil {
			return nil, err
		}
		return _left.Sub(ctx, w, right)
	})
}

func funMulti(ctx context.Context, w *World, args []Node) (Node, error) {
	type CanMulti interface {
		Node
		Multi(context.Context, *World, Node) (Node, error)
	}
	if len(args) == 1 {
		_, err := ExpectInterface[CanMulti](ctx, w, args[0], floatClass)
		if err != nil {
			return nil, err
		}
	}
	result, err := inject(args, func(left, right Node) (Node, error) {
		_left, err := ExpectInterface[CanMulti](ctx, w, left, floatClass)
		if err != nil {
			return nil, err
		}
		return _left.Multi(ctx, w, right)
	})
	if err != nil {
		return nil, err
	}
	if f, ok := result.(Float); ok {
		if f == 0 {
			for _, v := range args {
				if vv, ok := v.(Float); ok && vv == 0 {
					return f, nil
				}
				if vv, ok := v.(Integer); ok && vv == 0 {
					return f, nil
				}
			}
			return nil, &FloatingPointUnderflow{
				ArithmeticError: ArithmeticError{
					Operation: FunctionRef{value: &Function{F: funMulti}},
					Operands:  List(args...),
				}}
		}
		return checkFiniteFloat(float64(f), &Function{F: funMulti})
	}
	return result, nil
}

func div(z1, z2 int) int {
	q := z1 / z2
	r := z1 % z2
	if r != 0 && (z1 < 0) != (z2 < 0) {
		q--
	}
	return q
}

func divBig(z1, z2 BigInt) (Node, error) {
	if z2.Int.Sign() == 0 {
		return nil, &DivisionByZero{
			ArithmeticError: ArithmeticError{
				Operation: FunctionRef{value: &Function{C: 2, F: funDivide}},
				Operands:  List(z1, z2),
			},
		}
	}
	q := new(big.Int)
	r := new(big.Int)
	q.DivMod(z1.Int, z2.Int, r)
	if r.Sign() != 0 && (z1.Int.Sign() < 0) != (z2.Int.Sign() < 0) {
		q = new(big.Int).Sub(q, big.NewInt(1))
	}
	return BigInt{Int: q}, nil
}

func funDivide(ctx context.Context, w *World, args []Node) (Node, error) {
	if L, ok := args[0].(BigInt); ok {
		if R, ok := args[1].(BigInt); ok {
			return divBig(L, R)
		}
		if R, ok := args[1].(Integer); ok {
			return divBig(L, BigInt{Int: big.NewInt(int64(R))})
		}
	}
	L, err := ExpectClass[Integer](ctx, w, args[0])
	if err != nil {
		return nil, err
	}
	if R, ok := args[1].(BigInt); ok {
		return divBig(BigInt{Int: big.NewInt(int64(L))}, R)
	}
	R, err := ExpectClass[Integer](ctx, w, args[1])
	if err != nil {
		return nil, err
	}
	if R == 0 {
		return callHandler[Node](ctx, w, true, &DivisionByZero{
			ArithmeticError: ArithmeticError{
				Operation: FunctionRef{value: &Function{C: 2, F: funDivide}},
				Operands:  List(args[0], args[1]),
			},
		})
	}
	return Integer(div(int(L), int(R))), nil
}

type canLessThan interface {
	LessThan(context.Context, *World, Node) (bool, error)
	Node
}

func checkNumber(n Node) (canLessThan, error) {
	if v, ok := n.(BigInt); ok {
		return v, nil
	}
	if v, ok := n.(Integer); ok {
		return v, nil
	}
	if v, ok := n.(Float); ok {
		return v, nil
	}
	return nil, &DomainError{
		Object: n,
		Reason: "not a number",
	}
}

func checkNumbers(n ...Node) error {
	for _, v := range n {
		if _, err := checkNumber(v); err != nil {
			return err
		}
	}
	return nil
}

func funLessThan(ctx context.Context, w *World, args []Node) (Node, error) {
	return notNullToTrue(inject(args, func(left, right Node) (Node, error) {
		if err := checkNumbers(left, right); err != nil {
			return nil, err
		}
		_left, err := ExpectInterface[canLessThan](ctx, w, left, floatClass)
		if err != nil {
			return nil, err
		}
		result, err := _left.LessThan(ctx, w, right)
		if err != nil {
			return Null, err
		}
		if result {
			return right, nil
		}
		return Null, nil
	}))
}

func funGreaterThan(ctx context.Context, w *World, args []Node) (Node, error) {
	return notNullToTrue(inject(args, func(left, right Node) (Node, error) {
		if err := checkNumbers(left, right); err != nil {
			return nil, err
		}
		_right, err := ExpectInterface[canLessThan](ctx, w, right, floatClass)
		if err != nil {
			return nil, err
		}
		result, err := _right.LessThan(ctx, w, left)
		if err != nil {
			return Null, err
		}
		if result {
			return right, nil
		}
		return Null, nil
	}))
}

func boolToNode(b bool) (Node, error) {
	if b {
		return True, nil
	}
	return Null, nil
}

func funEqualOp(ctx context.Context, w *World, left, right Node) (Node, error) {
	return promoteOperands(ctx, w, left, right,
		func(a, b Integer) (Node, error) {
			return boolToNode(a == b)
		},
		func(a, b BigInt) (Node, error) {
			return boolToNode(a.Int.Cmp(b.Int) == 0)
		},
		func(a, b Float) (Node, error) {
			return boolToNode(a == b)
		},
		func(a, b *big.Float) (Node, error) {
			return boolToNode(a.Cmp(b) == 0)
		})
}

func funNotEqual(ctx context.Context, w *World, left, right Node) (Node, error) {
	v, err := funEqualOp(ctx, w, left, right)
	if err != nil {
		return nil, err
	}
	if IsNone(v) {
		return True, nil
	}
	return Null, nil
}

func funGreaterOrEqual(ctx context.Context, w *World, args []Node) (Node, error) {
	return notNullToTrue(inject(args, func(left, right Node) (Node, error) {
		if err := checkNumbers(left, right); err != nil {
			return nil, err
		}
		//     left >= right
		// <=> not (left < right )
		_left, err := ExpectInterface[canLessThan](ctx, w, left, floatClass)
		if err != nil {
			return nil, err
		}
		result, err := _left.LessThan(ctx, w, right)
		if err != nil {
			return Null, err
		}
		if result {
			return Null, nil
		}
		return right, nil
	}))
}

func funLessOrEqual(ctx context.Context, w *World, args []Node) (Node, error) {
	return notNullToTrue(inject(args, func(left, right Node) (Node, error) {
		if err := checkNumbers(left, right); err != nil {
			return nil, err
		}
		//     left <= right
		// <=> not (right < left)
		_right, err := ExpectInterface[canLessThan](ctx, w, right, floatClass)
		if err != nil {
			return nil, err
		}
		result, err := _right.LessThan(ctx, w, left)
		if err != nil {
			return Null, err
		}
		if result {
			return Null, nil
		}
		return right, nil
	}))
}

func cmdAnd(ctx context.Context, w *World, param Node) (Node, error) {
	var value Node = True
	for IsSome(param) {
		var err error

		value, param, err = w.ShiftAndEvalCar(ctx, param)
		if err != nil {
			return nil, err
		}
		if IsNone(value) {
			return Null, nil
		}
	}
	return value, nil
}

func cmdOr(ctx context.Context, w *World, param Node) (Node, error) {
	var value Node = Null
	for IsSome(param) {
		var value Node
		var err error

		value, param, err = w.ShiftAndEvalCar(ctx, param)
		if err != nil {
			return nil, err
		}
		if IsSome(value) {
			return value, nil
		}
	}
	return value, nil
}

func floatToInteger(ctx context.Context, w *World, arg Node, f func(float64) float64) (Node, error) {
	if value, ok := arg.(Integer); ok {
		return value, nil
	}
	value, err := ExpectClass[Float](ctx, w, arg)
	if err != nil {
		return nil, err
	}
	return Integer(int64(f(float64(value)))), nil
}

// funTruncate implements (truncte X). It returns the integer value of X.
func funTruncate(ctx context.Context, w *World, arg Node) (Node, error) {
	return floatToInteger(ctx, w, arg, math.Trunc)
}

// funFloor implements (truncte X). It returns the greatest integer value less than or equal to x.
func funFloor(ctx context.Context, w *World, arg Node) (Node, error) {
	return floatToInteger(ctx, w, arg, math.Floor)
}

// funCeiling implements (ceiling X). It returns the least integer value greater than or equal to x.
func funCeiling(ctx context.Context, w *World, arg Node) (Node, error) {
	return floatToInteger(ctx, w, arg, math.Ceil)
}

func funRound(ctx context.Context, w *World, arg Node) (Node, error) {
	return floatToInteger(ctx, w, arg, math.RoundToEven)
}

func mod(z1, z2 int) int {
	r := z1 % z2
	if r != 0 && (r < 0) != (z2 < 0) {
		r += z2
	}
	return r
}

func modBig(z1, z2 BigInt) (Node, error) {
	if z2.Int.Sign() == 0 {
		return nil, &DivisionByZero{
			ArithmeticError: ArithmeticError{
				Operation: FunctionRef{value: &Function{C: 2, F: funDivide}},
				Operands:  List(z1, z2),
			},
		}
	}
	q := new(big.Int)
	r := new(big.Int)
	q.DivMod(z1.Int, z2.Int, r)
	if r.Sign() != 0 && (r.Sign() < 0) != (z2.Int.Sign() < 0) {
		r = new(big.Int).Add(r, z2.Int)
	}
	return BigInt{Int: r}, nil
}

func funMod(ctx context.Context, w *World, args []Node) (Node, error) {
	if L, ok := args[0].(BigInt); ok {
		if R, ok := args[1].(BigInt); ok {
			return modBig(L, R)
		} else if R, ok := args[1].(Integer); ok {
			return modBig(L, BigInt{Int: big.NewInt(int64(R))})
		}
	}
	L, err := ExpectClass[Integer](ctx, w, args[0])
	if err != nil {
		return nil, err
	}
	if R, ok := args[1].(BigInt); ok {
		return modBig(BigInt{Int: big.NewInt(int64(L))}, R)
	}
	R, err := ExpectClass[Integer](ctx, w, args[1])
	if err != nil {
		return nil, err
	}
	if R == 0 {
		return callHandler[Node](ctx, w, true, &DivisionByZero{
			ArithmeticError: ArithmeticError{
				Operation: FunctionRef{value: &Function{C: 2, F: funMod}},
				Operands:  List(args[0], args[1]),
			},
		})
	}
	return Integer(mod(int(L), int(R))), nil
}

func funRem(ctx context.Context, w *World, first, second Node) (Node, error) {
	if left, ok := first.(Integer); ok {
		if right, ok := second.(Integer); ok {
			return Integer(left % right), nil
		}
	}
	left, err := ExpectClass[Float](ctx, w, first)
	if err != nil {
		return nil, err
	}
	if right, ok := second.(Integer); ok {
		return Float(math.Mod(float64(left), float64(int(right)))), nil
	}
	right, err := ExpectClass[Float](ctx, w, second)
	if err != nil {
		return nil, err
	}
	return Float(math.Mod(float64(left), float64(right))), nil
}
