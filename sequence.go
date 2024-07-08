package gmnlisp

import (
	"context"
	"io"
	"strconv"
	"strings"
)

type Sequence interface {
	FirstAndRest() (Node, Node, bool)
	Node
}

var listClass = registerNewBuiltInClass[Sequence]("<list>")

func SeqEach(ctx context.Context, w *World, list Node, f func(Node) error) error {
	for IsSome(list) {
		seq, err := ExpectInterface[Sequence](ctx, w, list, listClass)
		if err != nil {
			return err
		}
		var value Node
		var ok bool

		value, list, ok = seq.FirstAndRest()
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

func (L *ListBuilder) Add(ctx context.Context, w *World, n Node) error {
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
	if L.last == nil {
		return Null
	}
	return L.first.Cdr
}

type StringBuilder struct {
	strings.Builder
}

func (S *StringBuilder) Add(ctx context.Context, w *World, n Node) error {
	r, err := ExpectClass[Rune](ctx, w, n)
	if err != nil {
		return err
	}
	S.WriteRune(rune(r))
	return nil
}

func (S *StringBuilder) Sequence() Node {
	return String(S.String())
}

func (S StringBuilder) GoString() string {
	return strconv.Quote(S.String())
}

func funElt(ctx context.Context, w *World, args []Node) (Node, error) {
	type canElt interface {
		Elt(int) (Node, error)
	}
	var value Node = args[0]
	for _, indexArg := range args[1:] {
		index, err := ExpectClass[Integer](ctx, w, indexArg)
		if err != nil {
			return nil, err
		}
		if aref, ok := value.(canElt); ok {
			var err error
			value, err = aref.Elt(int(index))
			if err != nil {
				return nil, err
			}
			continue
		}
		for list := value; index >= 0; {
			seq, ok := list.(Sequence)
			if !ok {
				return Null, nil
			}
			value, list, _ = seq.FirstAndRest()
			index--
		}
	}
	return value, nil
}

func funLength(ctx context.Context, w *World, arg Node) (Node, error) {
	length := 0
	err := SeqEach(ctx, w, arg, func(_ Node) error {
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
	_f, err := ExpectFunction(ctx, w, f)
	if err != nil {
		return err
	}
	listSet := make([]Node, len(sourceSet))
	copy(listSet, sourceSet)
	for {
		paramSet := make([]Node, len(listSet))
		for i := 0; i < len(listSet); i++ {
			if IsNone(listSet[i]) {
				return nil
			}
			seq, ok := listSet[i].(Sequence)
			if !ok {
				return ErrNotSupportType
			}
			paramSet[i], listSet[i], ok = seq.FirstAndRest()
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
	err := MapCar(ctx, w, argv[0], argv[1:], func(node Node) { buffer.Add(ctx, w, node) })
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
	_f, err := ExpectFunction(ctx, w, f)
	if err != nil {
		return err
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
			if IsNone(listSet[i]) {
				return nil
			}
			seq, ok := listSet[i].(Sequence)
			if !ok {
				return ErrNotSupportType
			}
			_, listSet[i], ok = seq.FirstAndRest()
			if !ok || IsNone(listSet[i]) {
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
	err := mapList(ctx, w, argv[0], argv[1:], func(n Node) { buffer.Add(ctx, w, n) })
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

func Reverse(list Node) (Node, error) {
	var result Node = Null
	for IsSome(list) {
		var car Node
		var err error

		car, list, err = Shift(list)
		if err != nil {
			return nil, err
		}
		result = &Cons{
			Car: car,
			Cdr: result,
		}
	}
	return result, nil
}

func NReverse(ctx context.Context, w *World, list Node) (Node, error) {
	var result Node = Null
	for IsSome(list) {
		cons, err := ExpectClass[*Cons](ctx, w, list)
		if err != nil {
			return nil, err
		}
		list = cons.Cdr
		cons.Cdr = result
		result = cons
	}
	return result, nil
}

func funReverse(_ context.Context, _ *World, arg Node) (Node, error) {
	return Reverse(arg)
}

type SeqBuilder interface {
	Add(context.Context, *World, Node) error
	Sequence() Node
}

func funSubSeq(ctx context.Context, w *World, args []Node) (Node, error) {
	start, err := ExpectClass[Integer](ctx, w, args[1])
	if err != nil {
		return nil, err
	}
	end, err := ExpectClass[Integer](ctx, w, args[2])
	if err != nil {
		return nil, err
	}
	var buffer SeqBuilder
	if _, ok := args[0].(String); ok {
		buffer = &StringBuilder{}
	} else {
		buffer = &ListBuilder{}
	}
	count := Integer(0)
	err = SeqEach(ctx, w, args[0], func(value Node) (e error) {
		if count >= end {
			return io.EOF
		}
		if start <= count {
			e = buffer.Add(ctx, w, value)
		}
		count++
		return
	})
	return buffer.Sequence(), ignoreEOF(err)
}

func funMember(ctx context.Context, w *World, expr, list Node) (Node, error) {
	for IsSome(list) {
		seq, err := ExpectInterface[Sequence](ctx, w, list, listClass)
		if err != nil {
			return nil, err
		}
		value, rest, ok := seq.FirstAndRest()
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
