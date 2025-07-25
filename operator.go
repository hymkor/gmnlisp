package gmnlisp

import (
	"context"
	"math"
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

func funDivide(ctx context.Context, w *World, args []Node) (Node, error) {
	L, err := ExpectClass[Integer](ctx, w, args[0])
	if err != nil {
		return nil, err
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

func funLessThan(ctx context.Context, w *World, args []Node) (Node, error) {
	return notNullToTrue(inject(args, func(left, right Node) (Node, error) {
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

func funEqualOp(ctx context.Context, w *World, args []Node) (Node, error) {
	for _, v := range args {
		if _, ok := v.(Integer); !ok {
			if _, err := ExpectClass[Float](ctx, w, v); err != nil {
				return nil, err
			}
		}
	}
	if args[0].Equals(args[1], EQUALP) {
		return True, nil
	}
	return Null, nil
}

func funGreaterOrEqual(ctx context.Context, w *World, args []Node) (Node, error) {
	return notNullToTrue(inject(args, func(left, right Node) (Node, error) {
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

func funMod(ctx context.Context, w *World, args []Node) (Node, error) {
	L, err := ExpectClass[Integer](ctx, w, args[0])
	if err != nil {
		return nil, err
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
