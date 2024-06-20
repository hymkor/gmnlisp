package gmnlisp

import (
	"context"
	"errors"
)

type _Method struct {
	types  []Class
	method func(context.Context, *World, []Node) (Node, error)
}

func (m *_Method) canCallWith(values []Node) bool {
	if len(m.types) != len(values) {
		return false
	}
	for i, t := range m.types {
		if !t.InstanceP(values[i]) {
			return false
		}
	}
	return true
}

type _Generic struct {
	Symbol
	argc    int
	methods []*_Method
}

func cmdDefGeneric(_ context.Context, w *World, node Node) (Node, error) {
	_name, node, err := Shift(node)
	if err != nil {
		return nil, err
	}
	name, ok := _name.(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}
	types, node, err := Shift(node)
	if err != nil {
		return nil, err
	}
	if IsSome(node) {
		return nil, ErrTooManyArguments
	}
	argc := 0
	for IsSome(types) {
		var type1 Node
		var err error
		type1, types, err = Shift(types)
		if err != nil {
			return nil, err
		}
		if _, ok := type1.(Symbol); !ok {
			return nil, ErrExpectedSymbol
		}
		argc++
	}
	w.DefineGlobal(name, &_Generic{Symbol: name, argc: argc})
	return name, nil
}

func (c *_Generic) Call(ctx context.Context, w *World, node Node) (Node, error) {
	values := []Node{}
	for IsSome(node) {
		var v Node
		var err error
		v, node, err = w.ShiftAndEvalCar(ctx, node)
		if err != nil {
			return nil, err
		}
		values = append(values, v)
	}
	for _, m := range c.methods {
		if m.canCallWith(values) {
			return m.method(ctx, w, values)
		}
	}
	return nil, errors.New("no match methods")
}

func cmdDefMethod(ctx context.Context, w *World, node Node) (Node, error) {
	_name, node, err := Shift(node)
	if err != nil {
		return nil, err
	}
	name, ok := _name.(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}
	_generic, err := w.Get(name)
	if err != nil {
		return nil, err
	}
	generic, ok := _generic.(*_Generic)
	if !ok {
		return nil, errors.New("Expected <generic>")
	}
	params, code, err := Shift(node)
	if err != nil {
		return nil, err
	}

	method := &_Method{}
	paramNames := []Symbol{}
	for IsSome(params) {
		var t1 Node
		t1, params, err = Shift(params)
		if err != nil {
			return nil, err
		}
		var _pn1 Node
		_pn1, t1, err = Shift(t1)
		if err != nil {
			return nil, err
		}
		pn1, ok := _pn1.(Symbol)
		if !ok {
			return nil, ErrExpectedSymbol
		}
		paramNames = append(paramNames, pn1)
		var _type1 Node
		_type1, t1, err = w.ShiftAndEvalCar(ctx, t1)
		if err != nil {
			return nil, err
		}
		if IsSome(t1) {
			return nil, ErrTooManyArguments
		}
		type1, ok := _type1.(Class)
		if !ok {
			return nil, errors.New("Expected type")
		}
		method.types = append(method.types, type1)
	}
	method.method = func(ctx context.Context, w *World, args []Node) (Node, error) {
		vars := Variables{}
		for i, v := range args {
			vars[paramNames[i]] = v
		}
		return Progn(ctx, w.Let(vars), code)
	}
	generic.methods = append(generic.methods, method)
	return name, nil
}

func funGenericFunctionP(ctx context.Context, w *World, node []Node) (Node, error) {
	if _, ok := node[0].(*_Generic); ok {
		return True, nil
	}
	return Null, nil
}
