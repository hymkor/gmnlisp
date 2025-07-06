package gmnlisp

import (
	"context"
	"fmt"
	"strconv"
	"unicode"
)

var (
	classList   = NewSymbol("<list>")
	classVector = NewSymbol("<general-vector>")
)

func cmdConvert(ctx context.Context, w *World, list Node) (Node, error) {

	source, list, err := w.ShiftAndEvalCar(ctx, list)
	if err != nil {
		return nil, err
	}
	class, list, err := Shift(list)
	if err != nil {
		return nil, err
	}
	if IsSome(list) {
		return nil, ErrTooManyArguments
	}

	if IsNone(source) {
		switch class {
		case arrayClass.name:
			return arrayClass.Create(), nil
		case stringClass.name:
			return String("NIL"), nil
		case symbolClass.name:
			return Null, nil
		case listClass.name:
			return Null, nil
		case classVector:
			return new(VectorBuilder).Sequence(), nil
		}
	}
	switch val := source.(type) {
	case Rune:
		switch class {
		case characterClass.name:
			return val, nil
		case integerClass.name:
			return Integer(val), nil
		case symbolClass.name:
			return NewSymbol(fmt.Sprintf("%c", val)), nil
		}
	case String:
		switch class {
		case integerClass.name:
			i, err := strconv.ParseInt(val.String(), 10, 64)
			if err != nil {
				return callHandler[*DomainError](ctx, w, true, &DomainError{
					Object:        val,
					ExpectedClass: integerClass,
				})
			}
			return Integer(int(i)), nil
		case floatClass.name:
			f, err := strconv.ParseFloat(val.String(), 64)
			if err != nil {
				return callHandler[*DomainError](ctx, w, true, &DomainError{
					Object:        val,
					ExpectedClass: floatClass,
				})
			}
			return Float(f), nil
		case symbolClass.name:
			return NewSymbol(val.String()), nil
		case stringClass.name:
			return val, nil
		case classList:
			var buffer ListBuilder
			for _, r := range val {
				buffer.Add(ctx, w, Rune(r))
			}
			return buffer.Sequence(), nil
		case classVector:
			var buffer VectorBuilder
			for _, r := range val {
				buffer.Add(ctx, w, Rune(r))
			}
			return buffer.Sequence(), nil
		}
	case Float:
		switch class {
		case floatClass.name:
			return val, nil
		case stringClass.name:
			return String(strconv.FormatFloat(float64(val), 'f', -1, 64)), nil
		}
	case Integer:
		switch class {
		case characterClass.name:
			if val < unicode.MaxRune && val != unicode.ReplacementChar {
				return Rune(val), nil
			}
		case integerClass.name:
			return val, nil
		case floatClass.name:
			return Float(val), nil
		case stringClass.name:
			return String(fmt.Sprintf("%d", int(val))), nil
		}
	case *Cons:
		switch class {
		case classList:
			return val, nil
		case classVector:
			var buffer VectorBuilder
			for IsSome(val) {
				buffer.Add(ctx, w, val.Car)
				var ok bool
				val, ok = val.Cdr.(*Cons)
				if !ok {
					break
				}
			}
			return buffer.Sequence(), nil
		}
	case *Array:
		if len(val.dim) != 1 {
			return nil, fmt.Errorf("%w: dimension is not 1: %#v", ErrNotSupportType, val)
		}
		switch class {
		case classList:
			var cons Node = nil
			for i := len(val.list) - 1; i >= 0; i-- {
				cons = &Cons{
					Car: val.list[i],
					Cdr: cons,
				}
			}
			return cons, nil
		}
	case Symbol:
		switch class {
		case stringClass.name:
			return String(val.OriginalString()), nil
		case symbolClass.name:
			return val, nil
		}
	}
	return nil, &DomainError{Object: source, ExpectedClass: BuiltInClassObject}
}

func cmdAssure(ctx context.Context, w *World, args Node) (Node, error) {
	first, args, err := Shift(args)
	if err != nil {
		return nil, err
	}
	second, args, err := w.ShiftAndEvalCar(ctx, args)
	if err != nil {
		return nil, err
	}
	if IsSome(args) {
		return nil, ErrTooManyArguments
	}
	classSymbol, err := ExpectSymbol(ctx, w, first)
	if err != nil {
		return nil, err
	}
	class, ok := w.class[classSymbol]
	if !ok {
		return nil, &DomainError{
			Object:        first,
			ExpectedClass: classClass,
		}
	}
	if !class.InstanceP(second) {
		return nil, &DomainError{
			Object:        second,
			ExpectedClass: class,
		}
	}
	return second, nil
}
