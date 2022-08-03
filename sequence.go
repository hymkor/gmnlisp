package gmnlisp

import (
	"context"
	"io"
	"math"
	"strings"
)

type _Sequence interface {
	firstAndRest() (Node, Node, bool)
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

type _SeqBuilder interface {
	Add(Node) error
	Sequence() Node
}

type _ListBuilder struct {
	first Cons
	last  *Cons
}

func (L *_ListBuilder) Add(n Node) error {
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
	return nil
}

func (L *_ListBuilder) Sequence() Node {
	return L.first.Cdr
}

type _StringBuilder struct {
	buffer strings.Builder
}

func (S *_StringBuilder) Add(n Node) error {
	r, ok := n.(Rune)
	if !ok {
		return ErrExpectedCharacter
	}
	S.buffer.WriteRune(rune(r))
	return nil
}

func (S *_StringBuilder) Sequence() Node {
	return String(S.buffer.String())
}

var sequenceBuilderTable = map[Symbol](func() _SeqBuilder){
	symbolForList:   func() _SeqBuilder { return &_ListBuilder{} },
	symbolForString: func() _SeqBuilder { return &_StringBuilder{} },
}

func newSeqBuilder(symbolNode Node) (_SeqBuilder, error) {
	symbol, ok := symbolNode.(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}
	builder, ok := sequenceBuilderTable[symbol]
	if !ok {
		return nil, ErrNotSupportType
	}
	return builder(), nil
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
	buffer, err := newSeqBuilder(list[0])
	if err != nil {
		return nil, err
	}
	for _, element := range list[1:] {
		err := seqEach(element, func(value Node) error {
			buffer.Add(value)
			return nil
		})
		if err != nil {
			return nil, err
		}
	}
	return buffer.Sequence(), nil
}

func funLength(_ context.Context, _ *World, argv []Node) (Node, error) {
	length := 0
	err := seqEach(argv[0], func(_ Node) error {
		length++
		return nil
	})
	return Integer(length), err
}

func mapCar(ctx context.Context, w *World, funcNode Node, listSet []Node, resultSet _SeqBuilder) error {
	f, err := funcNode.Eval(ctx, w)
	if err != nil {
		return err
	}
	_f, ok := f.(_Callable)
	if !ok {
		return ErrExpectedFunction
	}
	for {
		paramSet := make([]Node, len(listSet))
		for i := 0; i < len(listSet); i++ {
			if IsNull(listSet[i]) {
				return nil
			}
			seq, ok := listSet[i].(_Sequence)
			if !ok {
				return ErrNotSupportType
			}
			paramSet[i], listSet[i], ok = seq.firstAndRest()
			if !ok {
				return nil
			}
		}
		result, err := _f.Call(ctx, w, List(paramSet...))
		if err != nil {
			return err
		}
		resultSet.Add(result)
	}
}

func funMapCar(ctx context.Context, w *World, argv []Node) (Node, error) {
	if len(argv) < 1 {
		return nil, ErrTooFewArguments
	}
	var buffer _ListBuilder
	err := mapCar(ctx, w, argv[0], argv[1:], &buffer)
	return buffer.Sequence(), err
}

func funMap(ctx context.Context, w *World, argv []Node) (Node, error) {
	if len(argv) < 2 {
		return nil, ErrTooFewArguments
	}
	buffer, err := newSeqBuilder(argv[0])
	if err != nil {
		return nil, err
	}
	err = mapCar(ctx, w, argv[1], argv[2:], buffer)
	return buffer.Sequence(), err
}

func funCoerce(_ context.Context, _ *World, argv []Node) (Node, error) {
	buffer, err := newSeqBuilder(argv[1])
	if err != nil {
		return nil, ErrNotSupportType
	}
	err = seqEach(argv[0], func(value Node) error {
		buffer.Add(value)
		return nil
	})
	return buffer.Sequence(), err
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
		var buffer _StringBuilder
		err = seqEach(result, func(value Node) error {
			buffer.Add(value)
			return nil
		})
		return buffer.Sequence(), err
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

func funSubSeq(ctx context.Context, w *World, args []Node) (Node, error) {
	if len(args) < 2 {
		return nil, ErrTooFewArguments
	}
	if len(args) > 3 {
		return nil, ErrTooManyArguments
	}
	start, ok := args[1].(Integer)
	if !ok {
		return nil, ErrExpectedNumber
	}
	end := Integer(math.MaxInt)
	if len(args) >= 3 {
		end, ok = args[2].(Integer)
		if !ok {
			return nil, ErrExpectedNumber
		}
	}
	var buffer _SeqBuilder
	if _, ok := args[0].(String); ok {
		buffer = &_StringBuilder{}
	} else {
		buffer = &_ListBuilder{}
	}
	count := Integer(0)
	err := seqEach(args[0], func(value Node) (e error) {
		if count >= end {
			return io.EOF
		}
		if start <= count {
			e = buffer.Add(value)
		}
		count++
		return
	})
	return buffer.Sequence(), ignoreEOF(err)
}
