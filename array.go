package gmnlisp

import (
	"context"
	"errors"
	"fmt"
	"io"
	"strings"
	"unicode/utf8"
)

type VectorBuilder struct {
	list []Node
}

func (v *VectorBuilder) Add(ctx context.Context, w *World, value Node) error {
	v.list = append(v.list, value)
	return nil
}

func (v *VectorBuilder) Sequence() Node {
	return &Array{
		list: v.list,
		dim:  []int{len(v.list)},
	}
}

func NewVector(ctx context.Context, w *World, args ...Node) Node {
	var v VectorBuilder
	for _, value := range args {
		v.Add(ctx, w, value)
	}
	return v.Sequence()
}

func funCreateVector(ctx context.Context, w *World, args []Node) (Node, error) {
	n, err := ExpectClass[Integer](ctx, w, args[0])
	if err != nil {
		return nil, err
	}
	if n < 0 {
		condition := &DomainError{
			Object:        args[0],
			ExpectedClass: integerClass,
		}
		n, err = callHandler[Integer](ctx, w, true, condition)
		if err != nil {
			return nil, err
		}
	}
	if n >= exhaustThresHold {
		return callHandler[Node](ctx, w, false, StorageExhausted{})
	}
	v := &Array{
		list: make([]Node, int(n)),
		dim:  []int{int(n)},
	}
	var iniEle Node = Null
	if len(args) >= 2 {
		iniEle = args[1]
	}
	for i := 0; i < int(n); i++ {
		v.list[i] = iniEle
	}
	return v, nil
}

func funGeneralVectorP(ctx context.Context, w *World, arg Node) (Node, error) {
	A, ok := arg.(*Array)
	if !ok {
		return Null, nil
	}
	if len(A.dim) != 1 {
		return Null, nil
	}
	return True, nil
}

func funBasicVectorP(ctx context.Context, w *World, arg Node) (Node, error) {
	if _, ok := arg.(String); ok {
		return True, nil
	}
	return funGeneralVectorP(ctx, w, arg)
}

type Array struct {
	list []Node
	dim  []int
}

var (
	basicArrayClass       = registerNewAbstractClass[*Array]("<basic-array>")
	basicArrayStarClass   = registerNewAbstractClass[*Array]("<basic-array*>", basicArrayClass)
	generalArrayStarClass = registerClass(&BuiltInClass{
		name: NewSymbol("<general-array*>"),
		instanceP: func(n Node) bool {
			a, ok := n.(*Array)
			return ok && len(a.dim) != 1
		},
		create: func() Node { return nil },
		super:  []Class{ObjectClass, basicArrayClass, basicArrayStarClass},
	})
	basicVectorClass   = registerNewAbstractClass[*Array]("<basic-vector>", basicArrayClass)
	generalVectorClass = registerClass(&BuiltInClass{
		name: NewSymbol("<general-vector>"),
		instanceP: func(n Node) bool {
			a, ok := n.(*Array)
			return ok && len(a.dim) == 1
		},
		create: func() Node { return nil },
		super:  []Class{ObjectClass, basicArrayClass, basicVectorClass},
	})
)

func (a *Array) ClassOf() Class {
	if a != nil && len(a.dim) == 1 {
		return generalVectorClass
	}
	return generalArrayStarClass
}

func (A *Array) printTo(w io.Writer, mode PrintMode, list []Node, dim []int) ([]Node, int, error) {
	w.Write([]byte{'('})
	n := 0
	if len(dim) >= 1 {
		if i := 0; i < dim[0] {
			for {
				var _n int
				var err error
				if len(dim) >= 2 {
					list, _n, err = A.printTo(w, mode, list, dim[1:])
					n += _n
				} else {
					_n, err = tryPrintTo(w, list[0], mode)
					n += _n
					list = list[1:]
				}
				if err != nil {
					return nil, n, err
				}
				if i++; i >= dim[0] {
					break
				}
				_n, err = w.Write([]byte{' '})
				n += _n
				if err != nil {
					return nil, n, err
				}
			}
		}
	}
	_n, err := w.Write([]byte{')'})
	n += _n
	return list, n, err
}

func (A *Array) PrintTo(w io.Writer, mode PrintMode) (int, error) {
	var n int
	var err error

	if len(A.dim) <= 0 {
		// dimension zero
		n, err = fmt.Fprintf(w, "#%dA", len(A.dim))
		if err != nil {
			return 0, err
		}
		if len(A.list) >= 1 {
			var _n int
			_n, err = tryPrintTo(w, A.list[0], mode)
			n += _n
		}
		return n, err
	} else if len(A.dim) == 1 {
		n, err = w.Write([]byte{'#'})
	} else {
		n, err = fmt.Fprintf(w, "#%dA", len(A.dim))
	}
	if err != nil {
		return n, err
	}
	_, n, err = A.printTo(w, mode, A.list, A.dim)
	return n, err
}

func (t Array) String() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINC)
	return buffer.String()
}

func (t Array) GoString() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINT)
	return buffer.String()
}

func (A *Array) Equals(_B Node, mode EqlMode) bool {
	B, ok := _B.(*Array)
	if !ok {
		return false
	}
	if len(A.dim) != len(B.dim) {
		return false
	}
	for i, v := range A.dim {
		if v != B.dim[i] {
			return false
		}
	}
	if len(A.list) != len(B.list) {
		return false
	}
	for i, v := range A.list {
		if !v.Equals(B.list[i], mode) {
			return false
		}
	}
	return true
}

func funCreateArray(ctx context.Context, w *World, args []Node) (Node, error) {
	var dim Node
	var err error
	if IsSome(args[0]) {
		dim, err = ExpectClass[*Cons](ctx, w, args[0])
		if err != nil {
			return nil, err
		}
	}
	var ini Node = Null
	if len(args) >= 2 {
		ini = args[1]
	}
	_dim := make([]int, 0, 2)
	size := 1
	for IsSome(dim) {
		var _n Node
		var err error

		_n, dim, err = Shift(dim)
		if err != nil {
			return nil, err
		}
		n, err := ExpectClass[Integer](ctx, w, _n)
		if err != nil {
			return nil, err
		}
		if n < 0 {
			return callHandler[Node](ctx, w, false, &DomainError{
				Object:        Integer(n),
				ExpectedClass: integerClass,
			})
		}
		_dim = append(_dim, int(n))
		size *= int(n)
	}
	if size >= exhaustThresHold {
		return callHandler[Node](ctx, w, false, StorageExhausted{})
	}
	_list := make([]Node, size)
	for i := range _list {
		_list[i] = ini
	}
	return &Array{
		list: _list,
		dim:  _dim,
	}, nil
}

func dim2size(dim []int) int {
	size := 1
	for _, v := range dim {
		size *= v
	}
	return size
}

func (A *Array) Elt(n int) (Node, error) {
	if n < 0 || n >= A.dim[0] {
		return nil, ErrIndexOutOfRange
	}
	if len(A.dim) == 1 {
		return A.list[n], nil
	}
	size := dim2size(A.dim[1:])
	return &Array{
		list: A.list[size*n : size*(n+1)],
		dim:  A.dim[1:],
	}, nil
}

func funAref(ctx context.Context, w *World, args []Node) (Node, error) {
	if _, ok := args[0].(*Array); ok {
		return funGaref(ctx, w, args)
	}
	if _, ok := args[0].(String); ok {
		if len(args) > 2 {
			return nil, ErrTooManyArguments
		}
		if len(args) < 2 {
			return nil, ErrTooFewArguments
		}
		if i, ok := args[1].(Integer); ok && i < 0 {
			return callHandler[Node](ctx, w, false, &DomainError{
				Object:        args[1],
				ExpectedClass: integerClass,
			})
		}
		return funElt(ctx, w, args)
	}
	return callHandler[Node](ctx, w, false, &DomainError{
		Object:        args[0],
		ExpectedClass: basicArrayClass,
	})
}

func funGaref(ctx context.Context, w *World, args []Node) (Node, error) {
	array, err := ExpectClass[*Array](ctx, w, args[0])
	if err != nil {
		return nil, err
	}
	if len(args)-1 > len(array.dim) {
		return nil, ErrTooManyArguments
	}
	if len(args)-1 < len(array.dim) {
		return nil, ErrTooFewArguments
	}
	for _, _nth := range args[1:] {
		nth, err := ExpectClass[Integer](ctx, w, _nth)
		if err != nil {
			return nil, err
		}
		if nth < 0 {
			return callHandler[Node](ctx, w, false, &DomainError{
				Object:        nth,
				ExpectedClass: integerClass,
			})
		}
		if int(nth) >= array.dim[0] {
			return nil, ErrIndexOutOfRange
		}
		if len(array.dim) == 1 {
			return array.list[nth], nil
		}
		elementSize := dim2size(array.dim[1:])
		array = &Array{
			list: array.list[int(nth)*elementSize : int(nth+1)*elementSize],
			dim:  array.dim[1:],
		}
	}
	return array.list[0], nil
}

func funSetAref(ctx context.Context, w *World, args []Node) (Node, error) {
	if _, ok := args[1].(*Array); ok {
		return funSetGaref(ctx, w, args)
	}
	if s, ok := args[1].(String); ok {
		if len(args) > 3 {
			return nil, ErrTooManyArguments
		}
		if len(args) < 3 {
			return nil, ErrTooFewArguments
		}
		if i, ok := args[2].(Integer); !ok || i < 0 {
			return callHandler[Node](ctx, w, false, &DomainError{
				Object:        args[2],
				ExpectedClass: integerClass,
			})
		} else if L := utf8.RuneCountInString(string(s)); int(i) >= L {
			return nil, ErrIndexOutOfRange
		} else if _, ok := args[0].(Rune); !ok {
			return callHandler[Node](ctx, w, false, &DomainError{
				Object:        args[0],
				ExpectedClass: characterClass,
			})
		}
		return nil, errors.New("aref did not support <string>,yet")
	}
	return callHandler[Node](ctx, w, false, &DomainError{
		Object:        args[1],
		ExpectedClass: basicArrayClass,
	})
}
func funSetGaref(ctx context.Context, w *World, args []Node) (Node, error) {
	newValue := args[0]

	array, err := ExpectClass[*Array](ctx, w, args[1])
	if err != nil {
		return nil, err
	}

	if len(args)-2 > len(array.dim) {
		return nil, ErrTooManyArguments
	}

	for _, _index := range args[2:] {
		index, err := ExpectClass[Integer](ctx, w, _index)
		if err != nil {
			return nil, err
		}
		elementSize := dim2size(array.dim[1:])
		if int(index)*elementSize >= len(array.list) || index < 0 {
			condition := &DomainError{
				Object:        index,
				ExpectedClass: integerClass,
			}
			return callHandler[Node](ctx, w, false, condition)
		}
		if len(array.dim) == 1 {
			array.list[index] = newValue
			return newValue, nil
		}
		array = &Array{
			list: array.list[int(index)*elementSize : int(index+1)*elementSize],
			dim:  array.dim[1:],
		}
	}
	_newValue, err := ExpectClass[*Array](ctx, w, newValue)
	if err != nil {
		return nil, err
	}
	copy(array.list, _newValue.list)
	return _newValue, nil
}

func funArrayDimensions(ctx context.Context, w *World, arg Node) (Node, error) {
	if s, ok := arg.(String); ok {
		leng, err := funLength(ctx, w, s)
		return &Cons{Car: leng, Cdr: nil}, err
	}
	array, err := ExpectClass[*Array](ctx, w, arg)
	if err != nil {
		return nil, err
	}
	var cons Node = Null
	for i := len(array.dim) - 1; i >= 0; i-- {
		cons = &Cons{
			Car: Integer(array.dim[i]),
			Cdr: cons,
		}
	}
	return cons, nil
}

func funVector(_ context.Context, w *World, args []Node) (Node, error) {
	return &Array{
		list: args,
		dim:  []int{len(args)},
	}, nil
}

func funBasicArrayP(_ context.Context, w *World, arg Node) (Node, error) {
	if _, ok := arg.(*Array); ok {
		return True, nil
	}
	if _, ok := arg.(String); ok {
		return True, nil
	}
	return Null, nil
}

func funBasicArrayStarP(_ context.Context, w *World, arg Node) (Node, error) {
	if array, ok := arg.(*Array); ok && len(array.dim) >= 2 {
		return True, nil
	}
	return Null, nil
}

func funGeneralArrayStarP(_ context.Context, w *World, arg Node) (Node, error) {
	if array, ok := arg.(*Array); ok && len(array.dim) >= 2 {
		return True, nil
	}
	return Null, nil
}

func (A *Array) FirstAndRest() (Node, Node, bool) {
	if len(A.dim) <= 0 || len(A.list) <= 0 {
		return nil, nil, false
	}
	if len(A.dim) == 1 {
		return A.list[0], &Array{
			list: A.list[1:],
			dim:  []int{A.dim[0] - 1},
		}, true
	}
	elementSize := dim2size(A.dim[1:])
	return &Array{
			list: A.list[0:elementSize],
			dim:  A.dim[1:],
		}, &Array{
			list: A.list[elementSize:],
			dim:  append([]int{A.dim[0] - 1}, A.dim[1:]...),
		}, true
}
