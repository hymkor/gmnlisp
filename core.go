package gmnlisp

type Node interface {
	Equals(Node, EqlMode) bool
	String() string
	ClassOf() Class
}

type Class interface {
	Node
	Name() Symbol
	InstanceP(Node) bool
	Create() Node
	InheritP(Class) bool
	Supers() []Class
}

type EqlMode int

const (
	STRICT EqlMode = iota // corresponds to (eql A B)
	EQUAL                 // corresponds to (equal A B)
	EQUALP                // corresponds to (equalp A B)
)

type PrintMode int

const (
	PRINT PrintMode = iota // AS S-Expression
	PRINC                  // AS IS
)

type Symbol interface {
	Id() int
	Node
	OriginalString() string
}
