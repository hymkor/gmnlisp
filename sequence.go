package gmnlisp

import (
	"context"
	"fmt"
	"io"
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
	err := seqEach(list, func(value Node) error {
		r, ok := value.(Rune)
		if !ok {
			return fmt.Errorf("%w: `%s`", ErrExpectedCharacter, toString(value, PRINT))
		}
		buffer.WriteRune(rune(r))
		return nil
	})
	return String(buffer.String()), err
}

var coerceTable = map[Symbol]func(list Node) (Node, error){
	symbolForList:   coerceToList,
	symbolForString: coerceToString,
}

func seqEach(list Node, f func(Node) error) error {
	for HasValue(list) {
		seq, ok := list.(_Sequence)
		if !ok {
			return ErrExpectedSequence
		}
		var value Node

		value, list, ok = seq.firstAndRest()
		if !ok {
			break
		}
		if err := f(value); err != nil {
			return err
		}
	}
	return nil
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

type _ListBuilder struct {
	first Cons
	last  *Cons
}

func (L *_ListBuilder) Add(n Node) {
	tmp := &Cons{
		Car: n,
		Cdr: Null,
	}
	if L.last == nil {
		L.first.Cdr = tmp
	} else {
		L.last.Cdr = tmp
	}
	L.last = tmp
}

func (L *_ListBuilder) List() Node {
	return L.first.Cdr
}

func funConcatenate(ctx context.Context, w *World, list []Node) (Node, error) {
	if len(list) < 1 {
		return Null, nil
	}

	symbol, ok := list[0].(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}

	var buffer _ListBuilder

	for _, element := range list[1:] {
		err := seqEach(element, func(value Node) error {
			buffer.Add(value)
			return nil
		})
		if err != nil {
			return nil, err
		}
	}
	collector, ok := coerceTable[symbol]
	if !ok {
		return nil, ErrNotSupportType
	}
	return collector(buffer.List())
}

func funLength(_ context.Context, _ *World, argv []Node) (Node, error) {
	length := 0
	err := seqEach(argv[0], func(_ Node) error {
		length++
		return nil
	})
	return Integer(length), err
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
	var resultSet _ListBuilder
	for {
		paramSet := make([]Node, len(listSet))
		for i := 0; i < len(listSet); i++ {
			if IsNull(listSet[i]) {
				return resultSet.List(), nil
			}
			seq, ok := listSet[i].(_Sequence)
			if !ok {
				return nil, ErrNotSupportType
			}
			paramSet[i], listSet[i], ok = seq.firstAndRest()
			if !ok {
				return resultSet.List(), nil
			}
		}
		result, err := _f.Call(ctx, w, List(paramSet...))
		if err != nil {
			return nil, err
		}
		resultSet.Add(result)
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

func funReverse(_ context.Context, _ *World, argv []Node) (Node, error) {
	var result Node
	err := seqEach(argv[0], func(value Node) error {
		result = &Cons{
			Car: value,
			Cdr: result,
		}
		return nil
	})
	if err != nil {
		return nil, err
	}
	if _, ok := argv[0].(String); ok {
		return coerceToString(result)
	}
	return result, nil
}

func funMember(_ context.Context, _ *World, argv []Node) (Node, error) {
	expr := argv[0]
	list := argv[1]

	for HasValue(list) {
		seq, ok := list.(_Sequence)
		if !ok {
			return nil, ErrExpectedSequence
		}
		value, rest, ok := seq.firstAndRest()
		if !ok {
			break
		}
		if expr.Equals(value, EQUAL) {
			return list, nil
		}
		list = rest
	}
	return Null, nil
}

func funPosition(_ context.Context, _ *World, argv []Node) (Node, error) {
	expr := argv[0]
	list := argv[1]
	count := 0

	err := seqEach(list, func(value Node) error {
		if expr.Equals(value, EQUAL) {
			return io.EOF
		}
		count++
		return nil
	})
	if err == io.EOF {
		return Integer(count), nil
	}
	return Null, nil
}
