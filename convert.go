package gmnlisp

import (
	"context"
	"fmt"
	"strconv"
)

var (
	classString      = NewSymbol("<string>")
	classUTF8String  = NewSymbol("<utf8string>")
	classUTF32String = NewSymbol("<utf32string>")
	classSymbol      = NewSymbol("<symbol>")
	classInteger     = NewSymbol("<integer>")
	classFloat       = NewSymbol("<float>")
	classList        = NewSymbol("<list>")
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
	if HasValue(list) {
		return nil, ErrTooManyArguments
	}
	switch val := source.(type) {
	case Rune:
		switch class {
		case classInteger:
			return Integer(val), nil
		case classSymbol:
			return NewSymbol(fmt.Sprintf("%c", val)), nil
		}
	case UTF32String:
		switch class {
		case classString:
			fallthrough
		case classUTF8String:
			return UTF8String(val.String()), nil
		case classUTF32String:
			return val, nil
		case classList:
			var buffer _ListBuilder
			for _, r := range val {
				buffer.Add(Rune(r))
			}
			return buffer.Sequence(), nil
		}
	case UTF8String:
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
		case classSymbol:
			return NewSymbol(val.String()), nil
		case classString:
			fallthrough
		case classUTF8String:
			return val, nil
		case classUTF32String:
			return UTF32String(val.String()), nil
		case classList:
			var buffer _ListBuilder
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
		case classString:
			fallthrough
		case classUTF8String:
			return UTF8String(fmt.Sprintf("%f", float64(val))), nil
		case classUTF32String:
			return UTF32String(fmt.Sprintf("%f", float64(val))), nil
		}
	case Integer:
		switch class {
		case classInteger:
			return val, nil
		case classFloat:
			return Float(val), nil
		case classString:
			fallthrough
		case classUTF8String:
			return UTF8String(fmt.Sprintf("%d", int(val))), nil
		case classUTF32String:
			return UTF32String(fmt.Sprintf("%d", int(val))), nil
		}
	case *Cons:
		if class == classList {
			return val, nil
		}
	}
	return nil, ErrNotSupportType
}
