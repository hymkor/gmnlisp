package gmnlisp

import (
	"context"
	"errors"
	"fmt"
	"io"

	"github.com/hymkor/gmnlisp/pkg/parser"
)

var (
	ErrAbort                = errors.New("abort")
	ErrDevisionByZero       = errors.New("devision by zeor")
	ErrExpectedArray        = errors.New("expected array")
	ErrExpectedClass        = errors.New("expected class")
	ErrExpectedCons         = errors.New("expected cons")
	ErrExpectedHash         = errors.New("expected hashtable")
	ErrExpectedKeyword      = errors.New("expected keyword")
	ErrExpectedMacro        = errors.New("expected macro")
	ErrExpectedReader       = errors.New("expected reader")
	ErrExpectedSequence     = errors.New("expected sequence")
	ErrExpectedStream       = errors.New("expected stream")
	ErrExpectedSymbol       = errors.New("expected symbol")
	ErrExpectedSymbolOrList = errors.New("expected symbol or list")
	ErrExpectedWriter       = errors.New("expected writer")
	ErrIndexOutOfRange      = errors.New("index out of range")
	ErrInvalidFormat        = errors.New("invalid format")
	ErrNoMatchMethods       = errors.New("no match methods")
	ErrNotSupportType       = errors.New("not support type")
	ErrQuit                 = errors.New("bye")
	ErrVariableUnbound      = errors.New("unbound variable")

	ErrCanNotParseNumber = parser.ErrCanNotParseNumber
	ErrTooFewArguments   = parser.ErrTooFewArguments
	ErrTooManyArguments  = parser.ErrTooManyArguments
	ErrTooShortTokens    = parser.ErrTooShortTokens
)

type EqlMode int

const (
	STRICT EqlMode = iota
	EQUAL
	EQUALP
)

type PrintMode int

const (
	PRINT PrintMode = iota // AS S-Expression
	PRINC                  // AS IS
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

var builtInClass = newBuiltInClass[*_BuiltInClass]("<built-in-class>")

func (e *_BuiltInClass) ClassOf() Class {
	return builtInClass
}

func (e *_BuiltInClass) Name() Symbol {
	return e.name
}

func (e *_BuiltInClass) InstanceP(n Node) bool {
	return e.instanceP(n)
}

func (e *_BuiltInClass) String() string {
	return e.Name().String()
}

func (e *_BuiltInClass) GoString() string {
	return e.Name().String()
}

func (e *_BuiltInClass) PrintTo(w io.Writer, m PrintMode) (int, error) {
	return io.WriteString(w, e.String())
}

func (e *_BuiltInClass) Equals(_other Node, _ EqlMode) bool {
	other, ok := _other.(*_BuiltInClass)
	return ok && other.String() == e.String()
}

func (e *_BuiltInClass) Eval(context.Context, *World) (Node, error) {
	return e, nil
}

func funClassOf(_ context.Context, _ *World, argv []Node) (Node, error) {
	return argv[0].ClassOf(), nil
}

type Node interface {
	Eval(context.Context, *World) (Node, error)
	Equals(Node, EqlMode) bool
	PrintTo(io.Writer, PrintMode) (int, error)
	String() string
	GoString() string
	ClassOf() Class
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

func registerNewBuiltInClass[T Node](name string, super ...Class) *_BuiltInClass {
	class := newBuiltInClass[T](name)
	class.super = append(super, objectClass, builtInClass)
	autoLoadVars[class.name] = class
	return class
}

var objectClass = &_BuiltInClass{
	name:      NewSymbol("<object>"),
	instanceP: func(Node) bool { return true },
	create:    func() Node { return Null },
}

// IsNone returns whether `node` does not have a value or not
func IsNone(node Node) bool {
	if node == nil {
		return true
	}
	_, ok := node.(_NullType)
	return ok
}

// Deprecated: use IsNone
func IsNull(node Node) bool {
	return IsNone(node)
}

// IsSome returns whether `node` has a value or not
func IsSome(node Node) bool {
	if node == nil {
		return false
	}
	_, ok := node.(_NullType)
	return !ok
}

// Deprecated: use IsSome
func HasValue(node Node) bool {
	return IsSome(node)
}

func List(nodes ...Node) Node {
	var cons Node = Null
	for i := len(nodes) - 1; i >= 0; i-- {
		cons = &Cons{
			Car: nodes[i],
			Cdr: cons,
		}
	}
	return cons
}

func Shift(list Node) (Node, Node, error) {
	cons, ok := list.(*Cons)
	if !ok {
		return nil, nil, ErrTooFewArguments
	}
	return cons.getCar(), cons.Cdr, nil
}

func ListToArray(list Node, slice []Node) error {
	for i := 0; i < len(slice); i++ {
		var err error

		slice[i], list, err = Shift(list)
		if err != nil {
			return err
		}
	}
	if IsSome(list) {
		return ErrTooManyArguments
	}
	return nil
}

func checkContext(ctx context.Context) error {
	select {
	case <-ctx.Done():
		return ctx.Err()
	default:
		return nil
	}
}

func ignoreEOF(err error) error {
	if err == io.EOF {
		return nil
	}
	return err
}

func MakeError(e error, s any) error {
	return fmt.Errorf("%w: %#v", e, s)
}
