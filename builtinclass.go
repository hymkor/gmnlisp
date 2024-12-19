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

type _BuiltInClass struct {
	name      Symbol
	instanceP func(Node) bool
	create    func() Node
	super     []Class
}

func (e *_BuiltInClass) InheritP(c Class) bool {
	for _, s := range e.super {
		if s.Equals(c, STRICT) || s.InheritP(c) {
			return true
		}
	}
	return false
}

func (e *_BuiltInClass) Create() Node {
	return e.create()
}

var classClass = registerNewBuiltInClass[Class]("<class>")

var builtInClass = newAbstractClass[*_BuiltInClass]("<built-in-class>")

func (e *_BuiltInClass) ClassOf() Class {
	return builtInClass
}

func (e *_BuiltInClass) Name() Symbol {
	return e.name
}

func (e *_BuiltInClass) InstanceP(n Node) bool {
	return e.instanceP(n) || n.ClassOf().InheritP(e)
}

func (e *_BuiltInClass) String() string {
	return e.Name().String()
}

func (e *_BuiltInClass) Equals(_other Node, _ EqlMode) bool {
	other, ok := _other.(*_BuiltInClass)
	return ok && other.String() == e.String()
}

func funClassOf(_ context.Context, _ *World, arg Node) (Node, error) {
	return arg.ClassOf(), nil
}

var objectClass = &_BuiltInClass{
	name:      NewSymbol("<object>"),
	instanceP: func(Node) bool { return true },
	create:    func() Node { return nil },
}

func newBuiltInClass[T Node](name string) *_BuiltInClass {
	symbol := NewSymbol(name)
	return &_BuiltInClass{
		name: symbol,
		instanceP: func(n Node) bool {
			_, ok := n.(T)
			return ok
		},
		create: func() Node {
			var value T
			return value
		},
	}
}

func newAbstractClass[T Node](name string) *_BuiltInClass {
	symbol := NewSymbol(name)
	return &_BuiltInClass{
		name: symbol,
		instanceP: func(n Node) bool {
			_, ok := n.(T)
			return ok
		},
		create: func() Node {
			return nil
		},
	}
}

func registerClass(class *_BuiltInClass, super ...Class) Class {
	class.super = append(super, objectClass, builtInClass)
	autoLoadVars[class.name] = class
	return class
}

func registerNewBuiltInClass[T Node](name string, super ...Class) *_BuiltInClass {
	class := newBuiltInClass[T](name)
	class.super = append(super, objectClass, builtInClass)
	autoLoadVars[class.name] = class
	return class
}

func registerNewAbstractClass[T Node](name string, super ...Class) *_BuiltInClass {
	class := newAbstractClass[T](name)
	class.super = append(super, objectClass, builtInClass)
	autoLoadVars[class.name] = class
	return class
}
