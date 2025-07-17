package gmnlisp

import (
	"context"
	"errors"
	"fmt"
)

type methodType struct {
	restType Class
	types    []Class
	method   func(context.Context, *World, []Node) (Node, error)
}

func (m *methodType) canCallWith(values []Node) bool {
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

type genericType struct {
	Symbol
	argc    int
	rest    bool
	methods []*methodType
}

var genericFunction = registerClass(&BuiltInClass{
	name: NewSymbol("<generic-function>"),
	instanceP: func(n Node) bool {
		if f, ok := n.(FunctionRef); ok {
			_, ok := f.value.(*genericType)
			return ok
		}
		return false
	},
	create: func() Node { return nil },
	super:  []Class{ObjectClass, functionRefClassObject},
})

var standardGenericFunction = registerClass(&BuiltInClass{
	name: NewSymbol("<standard-generic-function>"),
	instanceP: func(n Node) bool {
		if f, ok := n.(FunctionRef); ok {
			if _, ok := f.value.(*genericType); ok {
				// do at later
				return ok
			}
		}
		return false
	},
	create: func() Node { return nil },
	super:  []Class{ObjectClass, functionRefClassObject, genericFunction},
})

func cmdDefGeneric(ctx context.Context, w *World, node Node) (Node, error) {
	_name, node, err := Shift(node)
	if err != nil {
		return nil, err
	}
	name, err := ExpectSymbol(ctx, w, _name)
	if err != nil {
		return nil, err
	}
	if f, ok := w.defun.Get(name); ok {
		if _, ok := f.(SpecialF); ok {
			return raiseProgramError(ctx, w, fmt.Errorf("%s: special operator can not be changed", name.String()))
		}
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
			if _, err := ExpectSymbol(ctx, w, type2); err != nil {
				return nil, fmt.Errorf("after %s %w", type1.String(), err)
			}
			if IsSome(types) {
				return nil, ErrTooManyArguments
			}
			hasRest = true
			break
		}
		_, err = ExpectSymbol(ctx, w, type1)
		if err != nil {
			return nil, fmt.Errorf("%s: %w", type1.String(), err)
		}
		argc++
	}
	w.defun.Set(name, &genericType{Symbol: name, argc: argc, rest: hasRest})
	return name, nil
}

func (c *genericType) Call(ctx context.Context, w *World, node Node) (Node, error) {
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
	return callHandler[FunctionRef](ctx, w, false, &_UndefinedEntity{
		name:  c.Symbol,
		space: symFunction,
	})
}

func (c *genericType) FuncId() uintptr {
	return funcToId(c)
}

func ExpectGeneric(c Callable) (*genericType, error) {
	g, ok := c.(*genericType)
	if !ok {
		return nil, errors.New("already used for normal function or macro")
	}
	return g, nil
}

func cmdDefMethod(ctx context.Context, w *World, node Node) (Node, error) {
	_name, node, err := Shift(node)
	if err != nil {
		return nil, err
	}
	name, err := ExpectSymbol(ctx, w, _name)
	if err != nil {
		return nil, err
	}
	_generic, err := w.GetFunc(name)
	if err != nil {
		return nil, err
	}
	generic, err := ExpectGeneric(_generic)
	if err != nil {
		return nil, fmt.Errorf("%w: %#v", err, name.String())
	}
	params, code, err := Shift(node)
	if err != nil {
		return nil, err
	}

	method := &methodType{}
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
			restName, err = ExpectSymbol(ctx, w, _name)
			if err != nil {
				return nil, err
			}
			_type1, t1, err := Shift(t1)
			if err != nil {
				return nil, err
			}
			if IsSome(t1) {
				return nil, ErrTooManyArguments
			}
			classSymbol, err := ExpectSymbol(ctx, w, _type1)
			if err != nil {
				return nil, err
			}
			type1, ok := w.class[classSymbol]
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
		pn1, err := ExpectSymbol(ctx, w, _pn1)
		if err != nil {
			return nil, err
		}
		paramNames = append(paramNames, pn1)
		var _type1 Node
		_type1, t1, err = Shift(t1)
		if err != nil {
			return nil, err
		}
		if IsSome(t1) {
			return nil, ErrTooManyArguments
		}
		classSymbol, err := ExpectSymbol(ctx, w, _type1)
		if err != nil {
			return nil, err
		}
		type1, ok := w.class[classSymbol]
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

func funGenericFunctionP(ctx context.Context, w *World, arg Node) (Node, error) {
	if f, ok := arg.(FunctionRef); ok {
		if _, okk := f.value.(*genericType); okk {
			return True, nil
		}
		return Null, nil
	}
	if _, ok := arg.(*genericType); ok {
		return True, nil
	}
	return Null, nil
}
