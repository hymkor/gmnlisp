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

var domainErrorClass = _embedClassOf[*DomainError]("<domain-error>")

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
		return nil, &DomainError{args[0], domainErrorClass}
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
