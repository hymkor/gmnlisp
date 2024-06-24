package gmnlisp

import (
	"context"
	"fmt"
)

type _Method struct {
	restType Class
	types    []Class
	method   func(context.Context, *World, []Node) (Node, error)
}

func (m *_Method) canCallWith(values []Node) bool {
	//println("values:", joinStringer(values, ":"))
	//println("method:", joinStringer(m.types, ":"))
	if m.restType != nil {
		//println("rest:", m.restType.String())
		if len(m.types) > len(values) {
			//println("too few arguments")
			return false
		}
	} else {
		if len(m.types) != len(values) {
			return false
		}
	}
	for i, t := range m.types {
		if !t.InstanceP(values[i]) {
			//println("NG2", t.String(), values[i].String())
			return false
		}
	}
	if m.restType != nil {
		for _, v := range values[len(m.types):] {
			if !m.restType.InstanceP(v) {
				//println("NG1")
				return false
			}
		}
	}
	return true
}

type _Generic struct {
	Symbol
	argc    int
	rest    bool
	methods []*_Method
}

func ExpectGeneric(value Node) (*_Generic, error) {
	return ExpectType[*_Generic](value, "<generic-function>")
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
	hasRest := false
	for IsSome(types) {
		var type1 Node
		var err error
		type1, types, err = Shift(types)
		if err != nil {
			return nil, err
		}
		if type1.Equals(colonRest, STRICT) || type1.Equals(ampRest, STRICT) {
			type2, types, err := Shift(types)
			if err != nil {
				return nil, err
			}
			if _, ok := type2.(Symbol); !ok {
				return nil, fmt.Errorf("after %s %w", type1.String(), ErrExpectedSymbol)
			}
			if IsSome(types) {
				return nil, ErrTooManyArguments
			}
			hasRest = true
			break
		}
		_, ok := type1.(Symbol)
		if !ok {
			return nil, fmt.Errorf("%s: %w", type1.String(), ErrExpectedSymbol)
		}
		argc++
	}
	w.DefineGlobal(name, &_Generic{Symbol: name, argc: argc, rest: hasRest})
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
	for i := len(c.methods) - 1; i >= 0; i-- {
		if m := c.methods[i]; m.canCallWith(values) {
			return m.method(ctx, w, values)
		}
	}
	return nil, ErrNoMatchMethods
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
		return nil, ErrExpectedGeneric
	}
	params, code, err := Shift(node)
	if err != nil {
		return nil, err
	}

	method := &_Method{}
	paramNames := []Symbol{}
	var restName Symbol
	for IsSome(params) {
		var t1 Node
		t1, params, err = Shift(params)
		if err != nil {
			return nil, err
		}
		if t1.Equals(colonRest, STRICT) || t1.Equals(ampRest, STRICT) {
			// rest
			t1, params, err = Shift(params)
			if err != nil {
				return nil, err
			}
			if IsSome(params) {
				return nil, ErrTooManyArguments
			}
			_name, t1, err = Shift(t1)
			if err != nil {
				return nil, err
			}
			restName, ok = _name.(Symbol)
			if !ok {
				return nil, ErrExpectedSymbol
			}
			_type1, t1, err := w.ShiftAndEvalCar(ctx, t1)
			if err != nil {
				return nil, err
			}
			if IsSome(t1) {
				return nil, ErrTooManyArguments
			}
			type1, ok := _type1.(Class)
			if !ok {
				return nil, ErrExpectedClass
			}
			method.restType = type1
			break
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
			return nil, ErrExpectedClass
		}
		method.types = append(method.types, type1)
	}
	method.method = func(ctx context.Context, w *World, args []Node) (Node, error) {
		vars := Variables{}
		for i, v := range args {
			if i >= len(paramNames) {
				var rest Node = Null
				for j := len(args) - 1; j >= i; j-- {
					rest = &Cons{Car: args[j], Cdr: rest}
				}
				vars[restName] = rest
				break
			}
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
