package gmnlisp

import (
	"context"
	"fmt"
	"io"
)

type DomainError struct {
	Object        Node
	ExpectedClass Class
}

var (
	domainErrorClass = _embedClassOf[*DomainError]("<domain-error>")
	symDomainError   = domainErrorClass.name
)

func (e *DomainError) ClassOf() Class {
	return domainErrorClass
}

func (e *DomainError) Equals(_other Node, mode EqlMode) bool {
	other, ok := _other.(*DomainError)
	if !ok {
		return false
	}
	return e.Object.Equals(other.Object, mode) &&
		e.ExpectedClass.Equals(other.ExpectedClass, mode)
}

func (e *DomainError) Eval(context.Context, *World) (Node, error) {
	return e, nil
}

func (e *DomainError) PrintTo(w io.Writer, mode PrintMode) (int, error) {
	return io.WriteString(w, e.String())
}

func (e *DomainError) String() string {
	return fmt.Sprintf("%#v: Expected %#v", e.Object, e.ExpectedClass)
}

func (e *DomainError) GoString() string {
	return e.String()
}

func (e *DomainError) Error() string {
	return e.String()
}

func funDomainErrorObject(ctx context.Context, w *World, args []Node) (Node, error) {
	e, ok := args[0].(*DomainError)
	if !ok {
		return nil, &DomainError{Object: args[0], ExpectedClass: domainErrorClass}
	}
	return e.Object, nil
}

func funDomainErrorExpectedClass(ctx context.Context, w *World, args []Node) (Node, error) {
	e, ok := args[0].(*DomainError)
	if !ok {
		return nil, &DomainError{e, domainErrorClass}
	}
	return e.ExpectedClass, nil
}

func ExpectType[T Node](_value Node, name string) (T, error) {
	value, ok := _value.(T)
	if ok {
		return value, nil
	}
	return value, &DomainError{
		Object:        _value,
		ExpectedClass: _embedClassOf[T](name),
	}
}

func ExpectString(_value Node) (String, error) {
	return ExpectType[String](_value, "<string>")
}

func ExpectInteger(_value Node) (Integer, error) {
	value, err := ExpectType[Integer](_value, "<integer>")
	if err == nil {
		return value, nil
	}
	if v, ok := _value.(Float); ok {
		return Integer(v), nil
	}
	return Integer(0), err
}

func ExpectFloat(_value Node) (Float, error) {
	value, err := ExpectType[Float](_value, "<float>")
	if err == nil {
		return value, nil
	}
	if v, ok := _value.(Integer); ok {
		return Float(v), nil
	}
	return Float(0.), err
}

func ExpectCharacter(_value Node) (Rune, error) {
	return ExpectType[Rune](_value, "<character>")
}

type _UndefinedFunction struct {
	name Symbol
}

func (u _UndefinedFunction) Error() string {
	return fmt.Sprintf("Undefined Function: %#v", u.name)
}

func (u _UndefinedFunction) String() string {
	return u.Error()
}

func (u _UndefinedFunction) GoString() string {
	return u.Error()
}

func (u _UndefinedFunction) Eval(_ context.Context, _ *World) (Node, error) {
	return u, nil
}

func (u _UndefinedFunction) PrintTo(w io.Writer, mode PrintMode) (int, error) {
	return fmt.Fprintf(w, "Undefined Function: %#v", u.name)
}

func (u _UndefinedFunction) Equals(other Node, mode EqlMode) bool {
	o, ok := other.(_UndefinedFunction)
	if !ok {
		return false
	}
	return u.name.Equals(o.name, mode)
}

var undefinedFunctionClass = registerNewBuiltInClass[_UndefinedFunction]("<undefined-function>")

func (u _UndefinedFunction) ClassOf() Class {
	return undefinedFunctionClass
}

var undefinedEntityName = &_Generic{
	Symbol: NewSymbol("undefined-entity-name"),
	argc:   1,
	methods: []*_Method{
		&_Method{
			types: []Class{undefinedFunctionClass},
			method: func(_ context.Context, _ *World, args []Node) (Node, error) {
				entity, ok := args[0].(_UndefinedFunction)
				if !ok {
					return nil, &DomainError{
						Object:        args[0],
						ExpectedClass: undefinedFunctionClass,
					}
				}
				return entity.name, nil
			},
		},
	},
}

var undefinedEntityNameSpace = &_Generic{
	Symbol: NewSymbol("undefined-entity-namespace"),
	argc:   1,
	methods: []*_Method{
		&_Method{
			types: []Class{undefinedFunctionClass},
			method: func(_ context.Context, _ *World, args []Node) (Node, error) {
				return NewSymbol("function"), nil
			},
		},
	},
}
