package gmnlisp

import (
	"context"
	"errors"
	"fmt"
	"sort"
)

type methodType struct {
	restType Class
	types    []Class
	method   func(context.Context, *World, []Node) (Node, error)
}

func (m *methodType) checkArguments(values []Node) (bool, error) {
	//println("values:", joinStringer(values, ":"))
	//println("method:", joinStringer(m.types, ":"))
	if len(m.types) > len(values) {
		return false, ErrTooFewArguments
	}
	if m.restType == nil {
		if len(m.types) < len(values) {
			return false, ErrTooManyArguments
		}
	}
	for i, t := range m.types {
		if !t.InstanceP(values[i]) {
			//println("NG2", t.String(), values[i].String())
			return false, nil
		}
	}
	if m.restType != nil {
		for _, v := range values[len(m.types):] {
			if !m.restType.InstanceP(v) {
				//println("NG1")
				return false, nil
			}
		}
	}
	return true, nil
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
		if isSpecial(f) {
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

var errNotSuperClass = errors.New("Not a super class")

func distanceClass(given, funcs Class) (int, bool) {
	if given.Name() == funcs.Name() {
		return 0, true
	}
	// givenClass can be assigned to funcClass
	supers := given.Supers()
	minimum := 99999
	found := false
	for i := len(supers) - 1; i >= 0; i-- {
		distance, ok := distanceClass(supers[i], funcs)
		if ok {
			found = true
			if distance < minimum {
				minimum = distance + 1
			}
		}
	}
	return minimum, found
}

func classLessThan(args []Node, left *methodType, right *methodType) (bool, error) {
	for i := 0; i < len(left.types); i++ {
		L, ok := distanceClass(args[i].ClassOf(), left.types[i])
		if !ok {
			return false, fmt.Errorf("%s: %s, %s", errNotSuperClass, args[i].ClassOf().String(), left.types[i].String())
		}
		R, ok := distanceClass(args[i].ClassOf(), right.types[i])
		if !ok {
			// return -1, errNotSuperClass
			return false, fmt.Errorf("%s: %s, %s", errNotSuperClass, args[i].ClassOf().String(), right.types[i].String())
		}
		if L < R {
			return true, nil
		} else if L > R {
			return false, nil
		}
	}
	return false, nil
}

var errNoMatchMethods = errors.New("no match methods")

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
	if len(values) < c.argc {
		return nil, ErrTooFewArguments
	}
	if !c.rest && len(values) > c.argc {
		return nil, ErrTooManyArguments
	}

	candidates := make([]*methodType, 0, len(c.methods))
	for i := len(c.methods) - 1; i >= 0; i-- {
		m := c.methods[i]
		ok, err := m.checkArguments(values)
		if ok && err == nil {
			candidates = append(candidates, m)
		}
	}
	if len(candidates) <= 0 {
		return nil, errNoMatchMethods
	}
	sort.Slice(candidates, func(i, j int) bool {
		lessThan, err := classLessThan(values, candidates[i], candidates[j])
		if err != nil {
			panic(err.Error())
		}
		return lessThan
	})
	currMethod := candidates[0]
	candidates = candidates[1:]

	var newWorld *World
	newWorld = &World{
		parent: w,
		shared: w.shared,
		funcs: Functions{
			NewSymbol("next-method-p"): Function0(func(context.Context, *World) (Node, error) {
				if len(candidates) > 0 {
					return True, nil
				}
				return Null, nil
			}),
			NewSymbol("call-next-method"): Function0(func(context.Context, *World) (Node, error) {
				if len(candidates) <= 0 {
					return nil, errors.New("no methods")
				}
				currMethod = candidates[0]
				candidates = candidates[1:]
				return currMethod.method(ctx, newWorld, values)
			}),
		},
	}
	return currMethod.method(ctx, newWorld, values)
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
