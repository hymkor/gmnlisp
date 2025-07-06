package gmnlisp

import (
	"context"
)

type Class interface {
	Node
	Name() Symbol
	InstanceP(Node) bool
	Create() Node
	InheritP(Class) bool
}

type BuiltInClass struct {
	name      Symbol
	instanceP func(Node) bool
	create    func() Node
	super     []Class
}

func (e *BuiltInClass) InheritP(c Class) bool {
	for _, s := range e.super {
		if s.Equals(c, STRICT) || s.InheritP(c) {
			return true
		}
	}
	return false
}

func (e *BuiltInClass) Create() Node {
	if e.create == nil {
		return nil
	}
	return e.create()
}

var classClass = registerNewBuiltInClass[Class]("<class>")

var builtInClass = NewAbstractClass[*BuiltInClass]("<built-in-class>")

func (e *BuiltInClass) ClassOf() Class {
	return builtInClass
}

func (e *BuiltInClass) Name() Symbol {
	return e.name
}

func (e *BuiltInClass) InstanceP(n Node) bool {
	return e.instanceP(n) || n.ClassOf().InheritP(e)
}

func (e *BuiltInClass) String() string {
	return e.Name().String()
}

func (e *BuiltInClass) Equals(_other Node, _ EqlMode) bool {
	other, ok := _other.(*BuiltInClass)
	return ok && other.String() == e.String()
}

func funClassOf(_ context.Context, _ *World, arg Node) (Node, error) {
	return arg.ClassOf(), nil
}

var ObjectClass = &BuiltInClass{
	name:      NewSymbol("<object>"),
	instanceP: func(Node) bool { return true },
	create:    func() Node { return nil },
}

func newBuiltInClass[T Node](name string, ctor func() Node, super ...Class) *BuiltInClass {
	return &BuiltInClass{
		name: NewSymbol(name),
		instanceP: func(n Node) bool {
			_, ok := n.(T)
			return ok
		},
		create: ctor,
		super:  super,
	}
}

func NewBuiltInClass[T Node](name string, super ...Class) *BuiltInClass {
	ctor := func() Node { var value T; return value }
	return newBuiltInClass[T](name, ctor, super...)
}

func NewAbstractClass[T Node](name string, super ...Class) *BuiltInClass {
	ctor := func() Node { return nil }
	return newBuiltInClass[T](name, ctor, super...)
}

func registerClass(class *BuiltInClass) *BuiltInClass {
	presetClass = append(presetClass, class)
	return class
}

func registerNewBuiltInClass[T Node](name string, super ...Class) *BuiltInClass {
	super = append(super, ObjectClass, builtInClass)
	return registerClass(NewBuiltInClass[T](name, super...))
}

func registerNewAbstractClass[T Node](name string, super ...Class) *BuiltInClass {
	super = append(super, ObjectClass, builtInClass)
	return registerClass(NewAbstractClass[T](name, super...))
}
