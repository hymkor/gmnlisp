package gmnlisp

import (
	"context"
	"fmt"
	"strings"
)

type _Sequence interface {
	firstAndRest() (Node, Node, bool)
}

const (
	symbolForNumber  = Symbol("number")
	symbolForInteger = Symbol("integer")
	symbolForFloat   = Symbol("float")
	symbolForString  = Symbol("string")
	symbolForSymbol  = Symbol("symbol")
	symbolForCons    = Symbol("cons")
	symbolForList    = Symbol("list")
)

func funTypep(_ context.Context, _ *World, args []Node) (Node, error) {
	symbol, ok := args[1].(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}
	ok = false
	switch Symbol(strings.ToLower(string(symbol))) {
	case symbolForNumber:
		_, ok = args[0].(Integer)
		if !ok {
			_, ok = args[0].(Float)
		}
	case symbolForInteger:
		_, ok = args[0].(Integer)
	case symbolForFloat:
		_, ok = args[0].(Float)
	case symbolForString:
		_, ok = args[0].(String)
	case symbolForSymbol:
		_, ok = args[0].(Symbol)
	case symbolForCons:
		_, ok = args[0].(*Cons)
	case symbolForList:
		ok = IsNull(args[0])
		if !ok {
			_, ok = args[0].(*Cons)
		}
	}
	if ok {
		return True, nil
	}
	return Null, nil
}

func coerceToList(list Node) (Node, error) {
	return list, nil
}

func coerceToString(list Node) (Node, error) {
	var buffer strings.Builder
	for HasValue(list) {
		var value Node

		seq, ok := list.(_Sequence)
		if !ok {
			return nil, ErrExpectedSequence
		}
		value, list, ok = seq.firstAndRest()
		if !ok {
			break
		}
		r, ok := value.(Rune)
		if !ok {
			return nil, fmt.Errorf("%w: `%s`", ErrExpectedCharacter, toString(value))
		}
		buffer.WriteRune(rune(r))
	}
	return String(buffer.String()), nil
}

var coerceTable = map[Symbol]func(list Node) (Node, error){
	symbolForList:   coerceToList,
	symbolForString: coerceToString,
}

func funAref(_ context.Context, _ *World, args []Node) (Node, error) {
	index, ok := args[1].(Integer)
	if !ok {
		return nil, ErrExpectedNumber
	}
	list := args[0]
	var value Node = Null
	for index >= 0 {
		seq, ok := list.(_Sequence)
		if !ok {
			return Null, nil
		}
		value, list, _ = seq.firstAndRest()
		index--
	}
	return value, nil
}

func funConcatenate(ctx context.Context, w *World, list []Node) (Node, error) {
	if len(list) < 1 {
		return Null, nil
	}

	symbol, ok := list[0].(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}

	first := &Cons{}
	last := first

	for _, element := range list[1:] {
		for HasValue(element) {
			seq, ok := element.(_Sequence)
			if !ok {
				return nil, ErrExpectedSequence
			}
			var value Node

			value, element, ok = seq.firstAndRest()
			if !ok {
				break
			}
			tmp := &Cons{
				Car: value,
				Cdr: Null,
			}
			last.Cdr = tmp
			last = tmp
		}
	}
	collector, ok := coerceTable[symbol]
	if !ok {
		return nil, ErrNotSupportType
	}
	return collector(first.Cdr)
}

func funLength(_ context.Context, _ *World, argv []Node) (Node, error) {
	length := 0
	target := argv[0]
	for HasValue(target) {
		seq, ok := target.(_Sequence)
		if !ok {
			return nil, ErrExpectedSequence
		}
		_, target, ok = seq.firstAndRest()
		if !ok {
			break
		}
		length++
	}
	return Integer(length), nil
}

func mapCar(ctx context.Context, w *World, funcNode Node, listSet []Node) (result Node, err error) {
	f, err := funcNode.Eval(ctx, w)
	if err != nil {
		return nil, err
	}
	_f, ok := f.(_Callable)
	if !ok {
		return nil, ErrExpectedFunction
	}
	resultFirst := &Cons{}
	resultLast := resultFirst
	for {
		paramSet := make([]Node, len(listSet))
		for i := 0; i < len(listSet); i++ {
			if IsNull(listSet[i]) {
				return resultFirst.Cdr, nil
			}
			seq, ok := listSet[i].(_Sequence)
			if !ok {
				return nil, ErrNotSupportType
			}
			paramSet[i], listSet[i], ok = seq.firstAndRest()
			if !ok {
				return resultFirst.Cdr, nil
			}
		}
		result, err := _f.Call(ctx, w, List(paramSet...))
		if err != nil {
			return nil, err
		}
		tmp := &Cons{Car: result, Cdr: Null}
		resultLast.Cdr = tmp
		resultLast = tmp
	}
}

func funMapCar(ctx context.Context, w *World, argv []Node) (Node, error) {
	if len(argv) < 1 {
		return nil, ErrTooFewArguments
	}
	return mapCar(ctx, w, argv[0], argv[1:])
}

func funMap(ctx context.Context, w *World, argv []Node) (Node, error) {
	if len(argv) < 2 {
		return nil, ErrTooFewArguments
	}
	symbol, ok := argv[0].(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}
	collector, ok := coerceTable[symbol]
	if !ok {
		return nil, ErrNotSupportType
	}
	result, err := mapCar(ctx, w, argv[1], argv[2:])
	if err != nil {
		return nil, err
	}
	return collector(result)
}

func funCoerce(_ context.Context, _ *World, argv []Node) (Node, error) {
	symbol, ok := argv[1].(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}
	collector, ok := coerceTable[symbol]
	if !ok {
		return nil, ErrNotSupportType
	}
	return collector(argv[0])
}
