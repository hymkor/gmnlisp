package gmnlisp

import (
	"context"
	"errors"
	"fmt"
	"io"
)

type DomainError struct {
	Object        Node
	ExpectedClass Class
}

var (
	domainErrorClass = newBuiltInClass[*DomainError]("<domain-error>")
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

func ExpectInterface[T Node](ctx context.Context, w *World, v Node, class Class) (T, error) {
	value, ok := v.(T)
	if ok {
		return value, nil
	}
	condition := &DomainError{
		Object:        v,
		ExpectedClass: class,
	}
	if w.handler != nil {
		_, e := w.handler.Call(ctx, w, UnevalList(condition))
		var ce *_ErrContinueCondition
		if errors.As(e, &ce) {
			if v, ok := ce.Value.(T); ok {
				return v, nil
			}
		}
	}
	return value, condition
}

func ExpectClass[T Node](ctx context.Context, w *World, v Node) (T, error) {
	class := v.ClassOf() // .ClassOf must be called even when value is nil
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

func (u *_UndefinedEntity) GoString() string {
	return u.Error()
}

func (u *_UndefinedEntity) Eval(_ context.Context, _ *World) (Node, error) {
	return u, nil
}

func (u *_UndefinedEntity) PrintTo(w io.Writer, mode PrintMode) (int, error) {
	return fmt.Fprintf(w, "undefined %s: %#v", u.space, u.name)
}

func (u *_UndefinedEntity) Equals(other Node, mode EqlMode) bool {
	o, ok := other.(*_UndefinedEntity)
	if !ok {
		return false
	}
	return u.space.Equals(o.space, mode) && u.name.Equals(o.name, mode)
}

var (
	undefinedEntityClass   = registerNewBuiltInClass[*_UndefinedEntity]("<undefined-entity>")
	unboundVariableClass   = registerNewBuiltInClass[*_UndefinedEntity]("<unbound-variable>", undefinedEntityClass)
	undefinedFunctionClass = registerNewBuiltInClass[*_UndefinedEntity]("<undefined-function>", undefinedEntityClass)
)

func (u *_UndefinedEntity) ClassOf() Class {
	if u.space.Equals(symVariable, STRICT) {
		return unboundVariableClass
	}
	return undefinedFunctionClass
}

func funUndefinedEntityName(ctx context.Context, w *World, args []Node) (Node, error) {
	entity, err := ExpectClass[*_UndefinedEntity](ctx, w, args[0])
	if err != nil {
		return nil, err
	}
	return entity.name, nil
}

func funUndefinedEntityNamespace(ctx context.Context, w *World, args []Node) (Node, error) {

	entity, err := ExpectClass[*_UndefinedEntity](ctx, w, args[0])
	if err != nil {
		return nil, err
	}
	return entity.space, nil
}
