package gmnlisp

import (
	"context"
	"errors"
	"strconv"
	"strings"
)

func cmdQuote(_ context.Context, _ *World, n Node) (Node, error) {
	var argv [1]Node
	if err := ListToArray(n, argv[:]); err != nil {
		return nil, err
	}
	return argv[0], nil
}

func quasiQuote(ctx context.Context, w *World, n Node) (Node, error) {
	if cons, ok := n.(*Cons); ok {
		if cons.Car == symUnquote {
			if cdrCons, ok := cons.Cdr.(*Cons); ok {
				newCar, err := w.Eval(ctx, cdrCons.Car)
				if err != nil {
					return nil, err
				}
				return newCar, nil //&Cons{Car: newCar, Cdr: newCdr}, nil
			}
			return w.Eval(ctx, cons.Cdr)
		}
		newCar, err := quasiQuote(ctx, w, cons.Car)
		if err != nil {
			return nil, err
		}
		newCdr, err := quasiQuote(ctx, w, cons.Cdr)
		if err != nil {
			return nil, err
		}
		return &Cons{Car: newCar, Cdr: newCdr}, nil
	}
	return n, nil
}

func cmdQuasiQuote(ctx context.Context, w *World, n Node) (Node, error) {
	value, _, err := Shift(n)
	if err != nil {
		return nil, err
	}
	return quasiQuote(ctx, w, value)
}

func funAtom(_ context.Context, _ *World, arg Node) (Node, error) {
	if _, ok := arg.(*Cons); ok {
		return Null, nil
	}
	return True, nil
}

func equalSub(ctx context.Context, w *World, list []Node, eq func(Node, Node) bool) (Node, error) {
	first := list[0]
	for _, next := range list[1:] {
		if !eq(first, next) {
			return Null, nil
		}
	}
	return True, nil
}

func funEq(ctx context.Context, w *World, list []Node) (Node, error) {
	return equalSub(ctx, w, list, func(left, right Node) bool {
		return left == right
	})
}

func funEql(ctx context.Context, w *World, list []Node) (Node, error) {
	return equalSub(ctx, w, list, func(left, right Node) bool {
		return left.Equals(right, STRICT)
	})
}

func funEqual(ctx context.Context, w *World, list []Node) (Node, error) {
	return equalSub(ctx, w, list, func(left, right Node) bool {
		if IsNone(left) {
			return IsNone(right)
		}
		return left.Equals(right, EQUAL)
	})
}

func funEqualp(ctx context.Context, w *World, list []Node) (Node, error) {
	return equalSub(ctx, w, list, func(left, right Node) bool {
		if IsNone(left) {
			return IsNone(right)
		}
		return left.Equals(right, EQUALP)
	})
}

func funNot(_ context.Context, w *World, arg Node) (Node, error) {
	if IsNone(arg) {
		return True, nil
	}
	return Null, nil
}

func funNotEqual(ctx context.Context, w *World, argv []Node) (Node, error) {
	for _, v := range argv {
		if _, ok := v.(Integer); !ok {
			if _, err := ExpectClass[Float](ctx, w, v); err != nil {
				return nil, err
			}
		}
	}
	if argv[0].Equals(argv[1], EQUALP) {
		return Null, nil
	}
	return True, nil
}

func xxxxP(arg Node, f1 func(Integer) bool, f2 func(Float) bool) (Node, error) {
	if value, ok := arg.(Integer); ok {
		if f1(value) {
			return True, nil
		}
	} else if value, ok := arg.(Float); ok {
		if f2(value) {
			return True, nil
		}
	}
	return Null, nil
}

func funZerop(_ context.Context, _ *World, arg Node) (Node, error) {
	return xxxxP(arg,
		func(value Integer) bool { return value == 0 },
		func(value Float) bool { return value == 0 })
}

func funNumberp(_ context.Context, _ *World, arg Node) (Node, error) {
	return xxxxP(arg,
		func(Integer) bool { return true },
		func(Float) bool { return true })
}

func funPlusp(_ context.Context, _ *World, arg Node) (Node, error) {
	return xxxxP(arg,
		func(value Integer) bool { return value > 0 },
		func(value Float) bool { return value > 0 })
}

func funMinusp(_ context.Context, _ *World, arg Node) (Node, error) {
	return xxxxP(arg,
		func(value Integer) bool { return value < 0 },
		func(value Float) bool { return value < 0 })
}

func funOddp(_ context.Context, _ *World, arg Node) (Node, error) {
	if value, ok := arg.(Integer); ok && value%2 == 1 {
		return True, nil
	}
	return Null, nil
}

func funEvenp(_ context.Context, _ *World, arg Node) (Node, error) {
	if value, ok := arg.(Integer); ok && value%2 == 0 {
		return True, nil
	}
	return Null, nil
}

func funNullp(_ context.Context, _ *World, arg Node) (Node, error) {
	if IsNone(arg) {
		return True, nil
	}
	return Null, nil
}

func funAnyTypep[T Node](_ context.Context, _ *World, arg Node) (Node, error) {
	if _, ok := arg.(T); ok {
		return True, nil
	}
	return Null, nil
}

type FloatingPointOverflow struct {
	ArithmeticError
}

var _ ArithmeticErrorInterface = &FloatingPointOverflow{}

func (f *FloatingPointOverflow) ClassOf() Class {
	return floatingPointOverflowClass
}

func (f *FloatingPointOverflow) Equals(other Node, m EqlMode) bool {
	if o, ok := other.(*FloatingPointOverflow); ok {
		return f.ArithmeticError.Equals(&o.ArithmeticError, m)
	}
	return false
}

type FloatingPointUnderflow struct {
	ArithmeticError
}

var _ ArithmeticErrorInterface = &FloatingPointUnderflow{}

func (f *FloatingPointUnderflow) ClassOf() Class {
	return floatingPointUnderflowClass
}

func (f *FloatingPointUnderflow) Equals(other Node, m EqlMode) bool {
	if o, ok := other.(*FloatingPointUnderflow); ok {
		return f.ArithmeticError.Equals(&o.ArithmeticError, m)
	}
	return false
}

var (
	floatingPointOverflowClass  = registerNewAbstractClass[*FloatingPointOverflow]("<floating-point-overflow>", ObjectClass, seriousConditionClass, errorClass, arithmeticErrorClass)
	floatingPointUnderflowClass = registerNewAbstractClass[*FloatingPointUnderflow]("<floating-point-underflow>", ObjectClass, seriousConditionClass, errorClass, arithmeticErrorClass)
)

func funParseNumber(ctx context.Context, w *World, arg Node) (Node, error) {
	s, err := ExpectClass[String](ctx, w, arg)
	if err != nil {
		return nil, err
	}
	val, ok, err := tryParseAsNumber(s.String())
	if !ok || err != nil {
		var numError *strconv.NumError
		if errors.As(err, &numError) {
			if numError.Func == "ParseFloat" {
				var cond Condition
				ae := &ArithmeticError{
					Operation: FunctionRef{value: Function1(funParseNumber)},
					Operands:  s,
				}
				if false || errors.Is(numError.Err, strconv.ErrRange) {
					cond = &FloatingPointOverflow{
						ArithmeticError: *ae,
					}
				} else if strings.Contains(numError.Num, "E-") || strings.Contains(numError.Num, "e-") {
					cond = &FloatingPointUnderflow{
						ArithmeticError: *ae,
					}
				} else {
					cond = &FloatingPointOverflow{
						ArithmeticError: *ae,
					}
				}
				return callHandler[*ArithmeticError](ctx, w, true, cond)
			}
		}
		return callHandler[*ParseError](ctx, w, true, &ParseError{
			str:           s,
			ExpectedClass: numberClass,
		})
	}
	return val, err
}
