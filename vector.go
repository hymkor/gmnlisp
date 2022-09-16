package gmnlisp

import (
	"context"
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

func (v Vector) Elt(n int) (Node, func(Node) error, error) {
	if n < 0 || n >= len(v) {
		return nil, nil, ErrIndexOutOfRange
	}
	return v[n], func(value Node) error {
		v[n] = value
		return nil
	}, nil
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
