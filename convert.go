package gmnlisp

import (
	"context"
	"fmt"
	"strconv"
)

var (
	classInteger   = NewSymbol("<integer>")
	classFloat     = NewSymbol("<float>")
	classList      = NewSymbol("<list>")
	classVector    = NewSymbol("<general-vector>")
	classCharacter = NewSymbol("<character>")
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
	switch val := source.(type) {
	case Rune:
		switch class {
		case classCharacter:
			return val, nil
		case classInteger:
			return Integer(val), nil
		case symbolClass.name:
			return NewSymbol(fmt.Sprintf("%c", val)), nil
		}
	case String:
		switch class {
		case classInteger:
			i, err := strconv.ParseInt(val.String(), 10, 64)
			if err != nil {
				return nil, err
			}
			return Integer(int(i)), nil
		case classFloat:
			f, err := strconv.ParseFloat(val.String(), 64)
			if err != nil {
				return nil, err
			}
			return Float(f), nil
		case symbolClass.name:
			return NewSymbol(val.String()), nil
		case stringClass.name:
			return val, nil
		case classList:
			var buffer ListBuilder
			for _, r := range val {
				buffer.Add(Rune(r))
			}
			return buffer.Sequence(), nil
		case classVector:
			var buffer VectorBuilder
			for _, r := range val {
				buffer.Add(Rune(r))
			}
			return buffer.Sequence(), nil
		}
	case Float:
		switch class {
		case classInteger: // it should be error
			return Integer(val), nil
		case classFloat:
			return val, nil
		case stringClass.name:
			return String(fmt.Sprintf("%f", float64(val))), nil
		}
	case Integer:
		switch class {
		case classCharacter:
			return Rune(val), nil
		case classInteger:
			return val, nil
		case classFloat:
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
				buffer.Add(val.Car)
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
			return String(val.String()), nil
		}
	}
	return nil, fmt.Errorf("%w: %#v to %#v", ErrNotSupportType, source, class)
}

func funAssure(ctx context.Context, w *World, args []Node) (Node, error) {
	class, ok := args[0].(Class)
	if !ok {
		return nil, &DomainError{
			Object:        args[0],
			ExpectedClass: classClass,
		}
	}
	if !class.InstanceP(args[1]) {
		return nil, &DomainError{
			Object:        args[1],
			ExpectedClass: class,
		}
	}
	return args[1], nil
}
