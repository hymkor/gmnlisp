package gmnlisp

import (
	"context"
	"errors"
	"fmt"
)

type ProgramError struct {
	err error
}

type DomainError struct {
	Object        Node
	ExpectedClass Class
}

var (
	programErrorClass = registerNewBuiltInClass[ProgramError]("<program-error>")
	domainErrorClass  = registerNewBuiltInClass[*DomainError]("<domain-error>", programErrorClass)
)

func (e ProgramError) ClassOf() Class {
	return programErrorClass
}

func (e ProgramError) Equals(n Node, _ EqlMode) bool {
	_n, ok := n.(ProgramError)
	if !ok {
		return false
	}
	return errors.Is(e.err, _n.err) || errors.Is(_n.err, e.err)
}

func (e ProgramError) String() string {
	return e.err.Error()
}

func (e ProgramError) Error() string {
	return e.err.Error()
}

func (e ProgramError) Unwrap() error {
	return e.err
}

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

func (e *DomainError) String() string {
	return fmt.Sprintf("%#v: Expected %#v", e.Object.String(), e.ExpectedClass.Name().String())
}

func (e *DomainError) Error() string {
	return e.String()
}

func funDomainErrorObject(ctx context.Context, w *World, arg Node) (Node, error) {
	e, err := ExpectClass[*DomainError](ctx, w, arg)
	if err != nil {
		return nil, err
	}
	return e.Object, nil
}

func funDomainErrorExpectedClass(ctx context.Context, w *World, arg Node) (Node, error) {
	e, err := ExpectClass[*DomainError](ctx, w, arg)
	if err != nil {
		return nil, err
	}
	return e.ExpectedClass, nil
}

type errorAndNode interface {
	error
	Node
}

func callHandler[T Node](ctx context.Context, w *World, cont bool, condition errorAndNode) (T, error) {
	if w.handler != nil {
		_, e := w.handler.Call(ctx, w, UnevalList(condition))
		var ce *_ErrContinueCondition
		if cont && errors.As(e, &ce) {
			if v, ok := ce.Value.(T); ok {
				return v, nil
			}
		}
	}
	var zero T
	return zero, condition
}

func ExpectInterface[T Node](ctx context.Context, w *World, v Node, class Class) (T, error) {
	if _v, ok := v.(Uneval); ok {
		v = _v.Node
	}
	value, ok := v.(T)
	if ok {
		return value, nil
	}
	condition := &DomainError{
		Object:        v,
		ExpectedClass: class,
	}
	return callHandler[T](ctx, w, true, condition)
}

func ExpectClass[T Node](ctx context.Context, w *World, v Node) (T, error) {
	var zero T
	class := zero.ClassOf() // .ClassOf must be called even when value is nil
	return ExpectInterface[T](ctx, w, v, class)
}

type _UndefinedEntity struct {
	name  Symbol
	space Symbol
}

func (u *_UndefinedEntity) Error() string {
	return fmt.Sprintf("undefined %s: %#v", u.space.String(), u.name.String())
}

func (u *_UndefinedEntity) String() string {
	return u.Error()
}

func (u *_UndefinedEntity) Equals(other Node, mode EqlMode) bool {
	o, ok := other.(*_UndefinedEntity)
	if !ok {
		return false
	}
	return u.space.Equals(o.space, mode) && u.name.Equals(o.name, mode)
}

var (
	undefinedEntityClass   = registerNewBuiltInClass[*_UndefinedEntity]("<undefined-entity>", programErrorClass)
	unboundVariableClass   = registerNewBuiltInClass[*_UndefinedEntity]("<unbound-variable>", undefinedEntityClass)
	undefinedFunctionClass = registerNewBuiltInClass[*_UndefinedEntity]("<undefined-function>", undefinedEntityClass)
)

func (u *_UndefinedEntity) ClassOf() Class {
	if u == nil {
		return undefinedEntityClass
	}
	if u.space.Equals(symVariable, STRICT) {
		return unboundVariableClass
	}
	return undefinedFunctionClass
}

func funUndefinedEntityName(ctx context.Context, w *World, arg Node) (Node, error) {
	entity, err := ExpectClass[*_UndefinedEntity](ctx, w, arg)
	if err != nil {
		return nil, err
	}
	return entity.name, nil
}

func funUndefinedEntityNamespace(ctx context.Context, w *World, arg Node) (Node, error) {

	entity, err := ExpectClass[*_UndefinedEntity](ctx, w, arg)
	if err != nil {
		return nil, err
	}
	return entity.space, nil
}
