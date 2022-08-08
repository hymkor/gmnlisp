package gmnlisp

import (
	"context"
	"io"
	"math"
	"strings"
)

type _Sequence interface {
	firstAndRest() (Node, Node, bool, func(Node) error)
}

func seqEach(list Node, f func(Node) error) error {
	for HasValue(list) {
		seq, ok := list.(_Sequence)
		if !ok {
			return ErrExpectedSequence
		}
		var value Node

		value, list, ok, _ = seq.firstAndRest()
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
	buffer []Rune
}

func (S *_StringBuilder) Add(n Node) error {
	r, ok := n.(Rune)
	if !ok {
		return ErrExpectedCharacter
	}
	S.buffer = append(S.buffer, r)
	return nil
}

func (S *_StringBuilder) Sequence() Node {
	return String(S.buffer)
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

func funAref(_ context.Context, _ *World, args []Node) (Node, func(Node) error, error) {
	index, ok := args[1].(Integer)
	if !ok {
		return nil, nil, ErrExpectedNumber
	}
	list := args[0]
	var value Node = Null
	var setter func(Node) error
	for index >= 0 {
		seq, ok := list.(_Sequence)
		if !ok {
			return Null, nil, nil
		}
		value, list, _, setter = seq.firstAndRest()
		index--
	}
	return value, setter, nil
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
			paramSet[i], listSet[i], ok, _ = seq.firstAndRest()
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

func getTestParameter(kwargs map[Keyword]Node) (func(context.Context, *World, Node, Node) (bool, error), error) {
	if test, ok := kwargs[":test"]; ok {
		caller, ok := test.(_Callable)
		if !ok {
			return nil, ErrExpectedFunction
		}
		return func(c context.Context, w *World, left, right Node) (bool, error) {
			result, err := caller.Call(c, w, List(left, right))
			return HasValue(result), err
		}, nil
	} else {
		return func(_ context.Context, _ *World, left, right Node) (bool, error) {
			return left.Equals(right, EQUAL), nil
		}, nil
	}
}

func funFind(c context.Context, w *World, argv []Node, kwargs map[Keyword]Node) (Node, error) {
	expr := argv[0]
	list := argv[1]
	test, err := getTestParameter(kwargs)
	if err != nil {
		return nil, err
	}
	var found Node

	err = seqEach(list, func(value Node) error {
		eq, err := test(c, w, expr, value)
		if err != nil {
			return err
		}
		if eq {
			found = value
			return io.EOF
		}
		return nil
	})
	if err == io.EOF {
		return found, nil
	}
	return Null, nil
}

func funMember(c context.Context, w *World, argv []Node, kwargs map[Keyword]Node) (Node, error) {
	expr := argv[0]
	list := argv[1]
	test, err := getTestParameter(kwargs)
	if err != nil {
		return nil, err
	}

	for HasValue(list) {
		seq, ok := list.(_Sequence)
		if !ok {
			return nil, ErrExpectedSequence
		}
		value, rest, ok, _ := seq.firstAndRest()
		if !ok {
			break
		}
		eq, err := test(c, w, expr, value)
		if err != nil {
			return nil, err
		}
		if eq {
			return list, nil
		}
		list = rest
	}
	return Null, nil
}

func funPosition(c context.Context, w *World, argv []Node, kwargs map[Keyword]Node) (Node, error) {
	expr := argv[0]
	list := argv[1]
	test, err := getTestParameter(kwargs)
	if err != nil {
		return nil, err
	}
	count := 0

	err = seqEach(list, func(value Node) error {
		eq, err := test(c, w, expr, value)
		if err != nil {
			return err
		}
		if eq {
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

func funSubSeq(ctx context.Context, w *World, args []Node) (Node, func(Node) error, error) {
	if len(args) < 2 {
		return nil, nil, ErrTooFewArguments
	}
	if len(args) > 3 {
		return nil, nil, ErrTooManyArguments
	}
	start, ok := args[1].(Integer)
	if !ok {
		return nil, nil, ErrExpectedNumber
	}
	end := Integer(math.MaxInt)
	if len(args) >= 3 {
		end, ok = args[2].(Integer)
		if !ok {
			return nil, nil, ErrExpectedNumber
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
	return buffer.Sequence(), func(newvalue Node) error {
		if s1, ok := args[0].(String); ok {
			s2, ok := newvalue.(String)
			if !ok {
				return ErrExpectedString
			}
			copy(s1[start:end], s2[:])
			return nil
		}
		count := Integer(0)
		list := args[0]
		for {
			var ok bool
			var setter func(Node) error

			wSeq, ok := list.(_Sequence)
			if !ok {
				return nil
			}
			_, list, ok, setter = wSeq.firstAndRest()
			if !ok {
				return nil
			}
			if count >= start && count < end {
				var newvalue1 Node
				rSeq, ok := newvalue.(_Sequence)
				if !ok {
					return ErrExpectedSequence
				}
				newvalue1, newvalue, ok, _ = rSeq.firstAndRest()
				if !ok {
					return nil
				}
				if err := setter(newvalue1); err != nil {
					return err
				}
			}
			count++
		}
	}, ignoreEOF(err)
}
