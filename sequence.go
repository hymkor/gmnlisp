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

func (S *StringBuilder) String() string {
	if S == nil {
		return ""
	}
	return S.Builder.String()
}

func (S *StringBuilder) Column() int {
	s := S.String()
	pos := strings.LastIndexByte(s, '\n')
	if pos >= 0 {
		return len(s) - pos - 1
	}
	return len(s)
}

func (S *StringBuilder) Add(ctx context.Context, w *World, n Node) error {
	r, err := ExpectClass[Rune](ctx, w, n)
	if err != nil {
		return err
	}
	S.WriteRune(rune(r))
	return nil
}

func (S *StringBuilder) Close() error {
	S.Reset()
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
	if _, ok := args[0].(canElt); !ok {
		return nil, &DomainError{
			Object: args[0],
			Reason: "Not a sequence",
		}
	}
	if a, ok := args[0].(*Array); ok && len(a.dim) != 1 {
		return nil, &DomainError{
			Object: args[0],
			Reason: "Not a vector or a list",
		}
	}
	var value Node = args[0]
	for i, indexArg := range args[1:] {
		index, err := ExpectClass[Integer](ctx, w, indexArg)
		if err != nil {
			return nil, err
		}
		if index < 0 {
			return callHandler[Node](ctx, w, false, &DomainError{
				Reason: "Not a non negative integer",
				Object: Integer(index),
			})
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
			if list == String("") {
				return callHandler[Node](ctx, w, false, &DomainError{
					Reason: "Index Out of Range",
					Object: args[i+1],
				})
			}
			seq, ok := list.(Sequence)
			if !ok {
				return callHandler[Node](ctx, w, false, &DomainError{
					Reason: "Index Out of Range",
					Object: args[i+1],
				})
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
	for i, v := range sourceSet {
		var err error
		sourceSet[i], err = ExpectList(ctx, w, v)
		if err != nil {
			return err
		}
	}
	_f, err := ExpectFunction(ctx, w, funcNode)
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
			Car: &Cons{
				Car: quoteSymbol,
				Cdr: &Cons{
					Car: list[i],
					Cdr: Null,
				},
			},
			Cdr: cons,
		}
	}
	return cons
}

func mapList(ctx context.Context, w *World, funcNode Node, sourceSet []Node, store func(Node)) error {
	for i, v := range sourceSet {
		var err error
		sourceSet[i], err = ExpectList(ctx, w, v)
		if err != nil {
			return err
		}
	}
	_f, err := ExpectFunction(ctx, w, funcNode)
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

func funReverse(ctx context.Context, w *World, arg Node) (Node, error) {
	var err error
	arg, err = ExpectList(ctx, w, arg)
	if err != nil {
		return nil, err
	}
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
	count := Integer(0)
	if start < 0 {
		return nil, &DomainError{
			Object: Integer(start),
			Reason: "Index out of range",
		}
	}
	if start > end {
		return nil, &DomainError{
			Object: Integer(end),
			Reason: "Index out of range",
		}
	}

	var buffer SeqBuilder
	if array, ok := args[0].(*Array); ok {
		elementSize := dim2size(array.dim[1:])
		startElement := int(start) * elementSize
		endElement := int(end) * elementSize
		if endElement > len(array.list) {
			return nil, &DomainError{
				Object: Integer(end),
				Reason: "Index out of range",
			}
		}
		return &Array{
			list: array.list[startElement:endElement],
			dim:  append([]int{int(end - start)}, array.dim[1:]...),
		}, nil
	} else if _, ok := args[0].(String); ok {
		buffer = &StringBuilder{}
	} else {
		if _, err := ExpectInterface[Sequence](ctx, w, args[0], listClass); err != nil {
			return nil, err
		}
		buffer = &ListBuilder{}
	}
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
	if end > count {
		return nil, &DomainError{
			Object: Integer(end),
			Reason: "Index out of range",
		}
	}
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
