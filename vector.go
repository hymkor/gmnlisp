package gmnlisp

import (
	"context"
	"fmt"
	"io"
)

type Vector []Node

func (v Vector) Equals(_other Node, m EqlMode) bool {
	other, ok := _other.(Vector)
	if !ok {
		return false
	}
	if len(other) != len(v) {
		return false
	}
	for i, element := range v {
		if !element.Equals(other[i], m) {
			return false
		}
	}
	return true
}

func (v Vector) PrintTo(w io.Writer, m PrintMode) (int, error) {
	delim := "#("
	n := 0
	for _, element := range v {
		_n, err := io.WriteString(w, delim)
		n += _n
		if err != nil {
			return n, err
		}
		_n, err = element.PrintTo(w, m)
		n += _n
		if err != nil {
			return n, err
		}
		delim = " "
	}
	_n, err := io.WriteString(w, ")")
	n += _n
	return n, err
}

func (v Vector) Eval(ctx context.Context, w *World) (Node, error) {
	return v, nil
}

func funVector(ctx context.Context, w *World, args []Node) (Node, error) {
	return Vector(args), nil
}

func (v Vector) FirstAndRest() (Node, Node, bool, func(Node) error) {
	if len(v) <= 0 {
		return Null, Null, false, func(Node) error { return nil }
	}
	return v[0], Vector(v[1:]), true, func(value Node) error {
		v[0] = value
		return nil
	}
}

func (v Vector) Elt(n int) (Node, error) {
	if n < 0 || n >= len(v) {
		return nil, ErrIndexOutOfRange
	}
	return v[n], nil
}

type VectorBuilder struct {
	list []Node
}

func (v *VectorBuilder) Add(value Node) error {
	v.list = append(v.list, value)
	return nil
}

func (v *VectorBuilder) Sequence() Node {
	return Vector(v.list)
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
	n, err := fmt.Fprintf(w, "#%dA", len(A.dim))
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

func (A *Array) Eval(_ context.Context, W *World) (Node, error) {
	return A, nil
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
			return nil, fmt.Errorf("%w: %s", ErrExpectedNumber, ToString(_n, PRINT))
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

func (A *Array) Elt(n int) (Node, error) {
	if n < 0 || n >= A.dim[0] {
		return nil, ErrIndexOutOfRange
	}
	if len(A.dim) == 1 {
		return A.list[n], nil
	}
	size := 1
	for _, v := range A.dim[1:] {
		size *= v
	}
	return &Array{
		list: A.list[size*n : size*(n+1)],
		dim:  A.dim[1:],
	}, nil
}

func funSetArrayElt(ctx context.Context, w *World, args []Node) (Node, error) {
	newValue := args[0]

	array, ok := args[1].(*Array)
	if !ok {
		return nil, fmt.Errorf("%w: %s", ErrExpectedArray, ToString(args[0], PRINT))
	}

	if len(args)-2 > len(array.dim) {
		return nil, ErrTooManyArguments
	}

	for _, _index := range args[2:] {
		index, ok := _index.(Integer)
		if !ok {
			return nil, fmt.Errorf("%w: %s", ErrExpectedNumber, ToString(_index, PRINT))
		}
		if len(array.dim) == 1 {
			array.list[index] = newValue
			return newValue, nil
		}
		elementSize := 1
		for _, d := range array.dim[1:] {
			elementSize *= d
		}
		array = &Array{
			list: array.list[int(index)*elementSize:],
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
