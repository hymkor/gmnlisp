package gmnlisp

import (
	"context"
	"fmt"
	"io"
)

type VectorBuilder struct {
	list []Node
}

func (v *VectorBuilder) Add(value Node) error {
	v.list = append(v.list, value)
	return nil
}

func (v *VectorBuilder) Sequence() Node {
	return &Array{
		list: v.list,
		dim:  []int{len(v.list)},
	}
}

func NewVector(args ...Node) Node {
	var v VectorBuilder
	for _, value := range args {
		v.Add(value)
	}
	return v.Sequence()
}

type Array struct {
	list []Node
	dim  []int
}

func (A *Array) printTo(w io.Writer, mode PrintMode, list []Node, dim []int) ([]Node, int, error) {
	dem := byte('(')
	n := 0
	for i := 0; i < dim[0]; i++ {
		_n, err := w.Write([]byte{dem})
		n += _n
		if err != nil {
			return nil, n, err
		}
		dem = ' '

		if len(dim) >= 2 {
			list, _n, err = A.printTo(w, mode, list, dim[1:])
			n += _n
		} else {
			_n, err = list[0].PrintTo(w, mode)
			n += _n
			list = list[1:]
		}
		if err != nil {
			return nil, n, err
		}
	}
	_n, err := w.Write([]byte{')'})
	n += _n
	return list, n, err
}

func (A *Array) PrintTo(w io.Writer, mode PrintMode) (int, error) {
	var n int
	var err error
	if len(A.dim) == 1 {
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
		if v != B.list[i] {
			return false
		}
	}
	return true
}

func (A *Array) Eval(ctx context.Context, w *World) (Node, error) {
	result := &Array{
		list: make([]Node, len(A.list)),
		dim:  make([]int, len(A.dim)),
	}
	for i, eq := range A.list {
		value, err := eq.Eval(ctx, w)
		if err != nil {
			return nil, err
		}
		result.list[i] = value
	}
	copy(result.dim, A.dim)
	return result, nil
}

func funCreateArray(ctx context.Context, w *World, args []Node) (Node, error) {
	dim := args[0]
	ini := args[1]

	_dim := make([]int, 0, 2)
	size := 1
	for HasValue(dim) {
		var _n Node
		var err error

		_n, dim, err = Shift(dim)
		if err != nil {
			return nil, err
		}
		n, ok := _n.(Integer)
		if !ok {
			return nil, fmt.Errorf("%w: %#v", ErrExpectedNumber, _n)
		}
		_dim = append(_dim, int(n))
		size *= int(n)
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
	array, ok := args[0].(*Array)
	if !ok {
		return nil, fmt.Errorf("%w: %#v", ErrExpectedArray, args[0])
	}
	if len(args)-1 > len(array.dim) {
		return nil, ErrTooManyArguments
	}
	for _, _nth := range args[1:] {
		nth, ok := _nth.(Integer)
		if !ok {
			return nil, fmt.Errorf("%w: %#v", ErrExpectedNumber, args[1])
		}
		if nth < 0 || int(nth) >= array.dim[0] {
			return nil, fmt.Errorf("%w: %#v", ErrIndexOutOfRange, args[1])
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
	return array, nil
}

func funSetAref(ctx context.Context, w *World, args []Node) (Node, error) {
	newValue := args[0]

	array, ok := args[1].(*Array)
	if !ok {
		return nil, fmt.Errorf("%w: %#v", ErrExpectedArray, args[0])
	}

	if len(args)-2 > len(array.dim) {
		return nil, ErrTooManyArguments
	}

	for _, _index := range args[2:] {
		index, ok := _index.(Integer)
		if !ok {
			return nil, fmt.Errorf("%w: %#v", ErrExpectedNumber, _index)
		}
		if len(array.dim) == 1 {
			array.list[index] = newValue
			return newValue, nil
		}
		elementSize := dim2size(array.dim[1:])
		array = &Array{
			list: array.list[int(index)*elementSize : int(index+1)*elementSize],
			dim:  array.dim[1:],
		}
	}
	_newValue, ok := newValue.(*Array)
	if !ok {
		return nil, ErrExpectedArray
	}
	copy(array.list, _newValue.list)
	return _newValue, nil
}

func funArrayDimensions(ctx context.Context, w *World, args []Node) (Node, error) {
	array, ok := args[0].(*Array)
	if !ok {
		return nil, ErrExpectedArray
	}
	var cons Node
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
