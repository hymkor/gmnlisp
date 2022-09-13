package gmnlisp

import (
	"context"
	"io"
	"math"
	"strings"
)

type Sequence interface {
	FirstAndRest() (Node, Node, bool, func(Node) error)
}

func SeqEach(list Node, f func(Node) error) error {
	for HasValue(list) {
		seq, ok := list.(Sequence)
		if !ok {
			return ErrExpectedSequence
		}
		var value Node

		value, list, ok, _ = seq.FirstAndRest()
		if !ok {
			break
		}
		if err := f(value); err != nil {
			return err
		}
	}
	return nil
}

type ListBuilder struct {
	first Cons
	last  *Cons
}

func (L *ListBuilder) Add(n Node) error {
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

func (L *ListBuilder) Sequence() Node {
	return L.first.Cdr
}

type UTF32StringBuilder struct {
	buffer []Rune
}

func (S *UTF32StringBuilder) Add(n Node) error {
	r, ok := n.(Rune)
	if !ok {
		return ErrExpectedCharacter
	}
	S.buffer = append(S.buffer, r)
	return nil
}

func (S *UTF32StringBuilder) Sequence() Node {
	return UTF32String(S.buffer)
}

type UTF8StringBuilder struct {
	buffer strings.Builder
}

func (S *UTF8StringBuilder) Add(n Node) error {
	r, ok := n.(Rune)
	if !ok {
		return ErrExpectedCharacter
	}
	S.buffer.WriteRune(rune(r))
	return nil
}

func (S *UTF8StringBuilder) Sequence() Node {
	return UTF8String(S.buffer.String())
}

func funElt(_ context.Context, _ *World, args []Node) (Node, func(Node) error, error) {
	// ISLisp's (elt)
	type _Aref interface {
		Aref(int) (Node, func(Node) error, error)
	}

	index, ok := args[1].(Integer)
	if !ok {
		return nil, nil, ErrExpectedNumber
	}
	list := args[0]
	var value Node = Null
	var setter func(Node) error

	if aref, ok := list.(_Aref); ok {
		return aref.Aref(int(index))
	}

	for index >= 0 {
		seq, ok := list.(Sequence)
		if !ok {
			return Null, nil, nil
		}
		value, list, _, setter = seq.FirstAndRest()
		index--
	}
	return value, setter, nil
}

func funLength(_ context.Context, _ *World, argv []Node) (Node, error) {
	length := 0
	err := SeqEach(argv[0], func(_ Node) error {
		length++
		return nil
	})
	return Integer(length), err
}

func MapCar(ctx context.Context, w *World, funcNode Node, sourceSet []Node, store func(Node)) error {
	f, err := funcNode.Eval(ctx, w)
	if err != nil {
		return err
	}
	_f, ok := f.(Callable)
	if !ok {
		return ErrExpectedFunction
	}
	listSet := make([]Node, len(sourceSet))
	copy(listSet, sourceSet)
	for {
		paramSet := make([]Node, len(listSet))
		for i := 0; i < len(listSet); i++ {
			if IsNull(listSet[i]) {
				return nil
			}
			seq, ok := listSet[i].(Sequence)
			if !ok {
				return ErrNotSupportType
			}
			paramSet[i], listSet[i], ok, _ = seq.FirstAndRest()
			if !ok {
				return nil
			}
		}
		result, err := _f.Call(ctx, w, listToQuotedList(paramSet))
		if err != nil {
			return err
		}
		store(result)
	}
}

func funMapCar(ctx context.Context, w *World, argv []Node) (Node, error) {
	if len(argv) < 1 {
		return nil, ErrTooFewArguments
	}
	var buffer ListBuilder
	err := MapCar(ctx, w, argv[0], argv[1:], func(node Node) { buffer.Add(node) })
	return buffer.Sequence(), err
}

func funMapC(ctx context.Context, w *World, argv []Node) (Node, error) {
	if len(argv) < 1 {
		return nil, ErrTooFewArguments
	}
	err := MapCar(ctx, w, argv[0], argv[1:], func(Node) {})
	if len(argv) < 2 {
		return Null, err
	}
	return argv[1], err
}

func funMapCan(ctx context.Context, w *World, argv []Node) (Node, error) {
	if len(argv) < 1 {
		return nil, ErrTooFewArguments
	}
	list := []Node{}
	err := MapCar(ctx, w, argv[0], argv[1:], func(node Node) { list = append(list, node) })
	if err != nil {
		return nil, err
	}
	return funAppend(ctx, w, list)
}

func listToQuotedList(list []Node) Node {
	var cons Node = Null
	for i := len(list) - 1; i >= 0; i-- {
		cons = &Cons{
			Car: newQuote(list[i]),
			Cdr: cons,
		}
	}
	return cons
}

func mapList(ctx context.Context, w *World, funcNode Node, sourceSet []Node, store func(Node)) error {
	f, err := funcNode.Eval(ctx, w)
	if err != nil {
		return err
	}
	_f, ok := f.(Callable)
	if !ok {
		return ErrExpectedFunction
	}
	listSet := make([]Node, len(sourceSet))
	copy(listSet, sourceSet)
	for {
		result, err := _f.Call(ctx, w, listToQuotedList(listSet))
		if err != nil {
			return err
		}
		store(result)

		for i := 0; i < len(listSet); i++ {
			if IsNull(listSet[i]) {
				return nil
			}
			seq, ok := listSet[i].(Sequence)
			if !ok {
				return ErrNotSupportType
			}
			_, listSet[i], ok, _ = seq.FirstAndRest()
			if !ok || IsNull(listSet[i]) {
				return nil
			}
		}
	}
}

func funMapList(ctx context.Context, w *World, argv []Node) (Node, error) {
	if len(argv) < 1 {
		return nil, ErrTooFewArguments
	}
	var buffer ListBuilder
	err := mapList(ctx, w, argv[0], argv[1:], func(n Node) { buffer.Add(n) })
	return buffer.Sequence(), err
}

func funMapL(ctx context.Context, w *World, argv []Node) (Node, error) {
	if len(argv) < 1 {
		return nil, ErrTooFewArguments
	}
	err := mapList(ctx, w, argv[0], argv[1:], func(Node) {})
	if len(argv) < 2 {
		return Null, err
	}
	return argv[0], err
}

func funMapCon(ctx context.Context, w *World, argv []Node) (Node, error) {
	if len(argv) < 1 {
		return nil, ErrTooFewArguments
	}
	list := []Node{}
	err := mapList(ctx, w, argv[0], argv[1:], func(n Node) { list = append(list, n) })
	if err != nil {
		return nil, err
	}
	return funAppend(ctx, w, list)
}

func funReverse(_ context.Context, _ *World, argv []Node) (Node, error) {
	var result Node
	err := SeqEach(argv[0], func(value Node) error {
		result = &Cons{
			Car: value,
			Cdr: result,
		}
		return nil
	})
	if err != nil {
		return nil, err
	}
	if _, ok := argv[0].(UTF32String); ok {
		var buffer UTF32StringBuilder
		err = SeqEach(result, func(value Node) error {
			buffer.Add(value)
			return nil
		})
		return buffer.Sequence(), err
	} else if _, ok := argv[0].(UTF8String); ok {
		var buffer UTF8StringBuilder
		err = SeqEach(result, func(value Node) error {
			buffer.Add(value)
			return nil
		})
		return buffer.Sequence(), err
	}
	return result, nil
}

type SeqBuilder interface {
	Add(Node) error
	Sequence() Node
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
	var buffer SeqBuilder
	if _, ok := args[0].(UTF32String); ok {
		buffer = &UTF32StringBuilder{}
	} else if _, ok := args[0].(UTF8String); ok {
		buffer = &UTF8StringBuilder{}
	} else if _, ok := args[0].(Vector); ok {
		buffer = &VectorBuilder{}
	} else {
		buffer = &ListBuilder{}
	}
	count := Integer(0)
	err := SeqEach(args[0], func(value Node) (e error) {
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
		if _, ok := args[0].(UTF8String); ok {
			return ErrNotSupportType
		}
		if s1, ok := args[0].(UTF32String); ok {
			s2, ok := newvalue.(UTF32String)
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

			wSeq, ok := list.(Sequence)
			if !ok {
				return nil
			}
			_, list, ok, setter = wSeq.FirstAndRest()
			if !ok {
				return nil
			}
			if count >= start && count < end {
				var newvalue1 Node
				rSeq, ok := newvalue.(Sequence)
				if !ok {
					return ErrExpectedSequence
				}
				newvalue1, newvalue, ok, _ = rSeq.FirstAndRest()
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

func funMember(c context.Context, w *World, argv []Node) (Node, error) {
	expr := argv[0]
	list := argv[1]

	for HasValue(list) {
		seq, ok := list.(Sequence)
		if !ok {
			return nil, ErrExpectedSequence
		}
		value, rest, ok, _ := seq.FirstAndRest()
		if !ok {
			break
		}
		if expr.Equals(value, STRICT) {
			return list, nil
		}
		list = rest
	}
	return Null, nil
}
