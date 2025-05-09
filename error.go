package gmnlisp

import (
	"context"
	"errors"
	"fmt"
)

type ErrorNode struct {
	Value error
}

var errorClass = &_BuiltInClass{
	name: NewSymbol("<error>"),
	instanceP: func(value Node) bool {
		_, ok := value.(error)
		return ok
	},
	create: func() Node {
		return nil
	},
	super: []Class{objectClass},
}

func (ErrorNode) ClassOf() Class {
	return errorClass
}

func (e ErrorNode) String() string {
	return e.Value.Error()
}

func (e ErrorNode) Equals(n Node, m EqlMode) bool {
	f, ok := n.(ErrorNode)
	if !ok {
		return false
	}
	return errors.Is(e.Value, f.Value) || errors.Is(f.Value, e.Value)
}

func (e ErrorNode) Error() string {
	return e.Value.Error()
}

func raiseError(ctx context.Context, w *World, e error) (Node, error) {
	if _, ok := e.(interface{ ClassOf() Class }); ok {
		return nil, e
	}
	condition := ErrorNode{Value: e}
	return callHandler[Node](ctx, w, false, condition)
}

type ProgramError struct {
	err error
}

type ControlError struct {
	err error
}

type DomainError struct {
	Object        Node
	ExpectedClass Class
}

type ParseError struct {
	str           String
	ExpectedClass Class
}

type StorageExhausted struct{}

type StreamError struct {
	Stream Node
}

type EndOfStream struct {
	Stream Node
}

var (
	programErrorClass     = registerNewAbstractClass[ProgramError]("<program-error>", errorClass)
	domainErrorClass      = registerNewAbstractClass[*DomainError]("<domain-error>", programErrorClass, errorClass)
	controlErrorClass     = registerNewAbstractClass[ControlError]("<control-error>", errorClass)
	parseErrorClass       = registerNewAbstractClass[*ParseError]("<parse-error>", errorClass)
	storageExhaustedClass = registerNewAbstractClass[StorageExhausted]("<storage-exhausted>", errorClass)
	streamErrorClass      = registerNewAbstractClass[StreamError]("<stream-error>", errorClass)
	endOfStreamClass      = registerNewAbstractClass[EndOfStream]("<end-of-stream>", streamErrorClass, errorClass)
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

func (e ControlError) ClassOf() Class {
	return controlErrorClass
}

func (e ControlError) Equals(n Node, _ EqlMode) bool {
	_n, ok := n.(ControlError)
	if !ok {
		return false
	}
	return errors.Is(e.err, _n.err) || errors.Is(_n.err, e.err)
}

func (e ControlError) String() string {
	return e.err.Error()
}

func (e ControlError) Error() string {
	return e.err.Error()
}

func (e ControlError) Unwrap() error {
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

type callHandlerLimitter struct{}

var errHandlerReturnNormally = errors.New("Handler return normally")

func callHandler[T Node](ctx context.Context, w *World, cont bool, condition errorAndNode) (T, error) {
	var zero T
	if len(w.handler) > 0 {
		if v := ctx.Value(callHandlerLimitter{}); v != nil {
			return zero, condition
		}
		ctx = context.WithValue(ctx, callHandlerLimitter{}, 1)
		for h := w.handler; len(h) > 0; h = h[:len(h)-1] {
			_, e := h[len(h)-1].Call(ctx, w, UnevalList(condition))
			var ce *_ErrContinueCondition
			if cont && errors.As(e, &ce) {
				if v, ok := ce.Value.(T); ok {
					return v, nil
				}
			}
			if IsNonLocalExists(e) {
				return zero, e
			}
			if e != nil {
				return zero, condition
			}
			condition = &ControlError{err: errHandlerReturnNormally}
		}
		return zero, &ControlError{err: errHandlerReturnNormally}
	}
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

func ExpectSymbol(ctx context.Context, w *World, v Node) (Symbol, error) {
	return ExpectInterface[Symbol](ctx, w, v, symbolClass)
}

func ExpectNonReservedSymbol(ctx context.Context, w *World, v Node) (_Symbol, error) {
	symbol, ok := v.(_Symbol)
	if ok {
		return symbol, nil
	}
	if _, err := ExpectSymbol(ctx, w, v); err != nil {
		return 0, err
	}
	_, err := raiseProgramError(ctx, w, errors.New("Expect Symbol"))
	return 0, err
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
	undefinedEntityClass   = registerNewAbstractClass[*_UndefinedEntity]("<undefined-entity>", programErrorClass)
	unboundVariableClass   = registerNewAbstractClass[*_UndefinedEntity]("<unbound-variable>", undefinedEntityClass)
	undefinedFunctionClass = registerNewAbstractClass[*_UndefinedEntity]("<undefined-function>", undefinedEntityClass)
)

func (u *_UndefinedEntity) ClassOf() Class {
	if u == nil {
		return undefinedEntityClass
	}
	if u.space.Equals(symVariable, STRICT) {
		return unboundVariableClass
	}
	if u.space.Equals(symDynamicVariable, STRICT) {
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

func funParseErrorString(ctx context.Context, w *World, arg Node) (Node, error) {
	entity, err := ExpectClass[*ParseError](ctx, w, arg)
	if err != nil {
		return nil, err
	}
	return String(entity.str), nil
}

func funParseErrorExpectedClass(ctx context.Context, w *World, arg Node) (Node, error) {
	entity, err := ExpectClass[*ParseError](ctx, w, arg)
	if err != nil {
		return nil, err
	}
	return entity.ExpectedClass, nil
}

func (p *ParseError) ClassOf() Class {
	return parseErrorClass
}

func (p *ParseError) String() string {
	return fmt.Sprintf("ParseError: %s: %s", String(p.str), p.ExpectedClass.String())
}

func (p *ParseError) Equals(other Node, m EqlMode) bool {
	o, ok := other.(*ParseError)
	if !ok {
		return false
	}
	return p.str == o.str && p.ExpectedClass.Equals(o.ExpectedClass, m)
}

func (p *ParseError) Error() string {
	return p.String()
}

func (s StorageExhausted) ClassOf() Class {
	return storageExhaustedClass
}

func (s StorageExhausted) Equals(n Node, _ EqlMode) bool {
	_, ok := n.(StorageExhausted)
	return ok
}

func (s StorageExhausted) String() string {
	return "storage exhausted"
}

func (s StorageExhausted) Error() string {
	return "storage exhausted"
}

func (s StreamError) ClassOf() Class {
	return streamErrorClass
}

func (s StreamError) Equals(other Node, m EqlMode) bool {
	o, ok := other.(StreamError)
	if !ok {
		return false
	}
	return s.Stream.Equals(o.Stream, m)
}

func (s StreamError) String() string {
	return "<stream-error>"
}

func (s StreamError) Error() string {
	return "<stream-error>"
}

func funStreamErrorStream(ctx context.Context, w *World, node Node) (Node, error) {
	if endOfStream, ok := node.(EndOfStream); ok {
		return endOfStream.Stream, nil
	}
	streamError, err := ExpectClass[StreamError](ctx, w, node)
	if err != nil {
		return nil, err
	}
	return streamError.Stream, nil
}

func (e EndOfStream) ClassOf() Class {
	return endOfStreamClass
}

func (e EndOfStream) Equals(n Node, _ EqlMode) bool {
	_, ok := n.(EndOfStream)
	return ok
}

func (e EndOfStream) String() string {
	return "<end-of-stream>"
}

func (e EndOfStream) Error() string {
	return "<end-of-stream>"
}

type SimpleError struct {
	FormatString    Node
	FormatArguments Node
}

var simpleErrorClass = registerNewAbstractClass[*SimpleError]("<simple-error>", errorClass)

func (s *SimpleError) ClassOf() Class {
	return simpleErrorClass
}

func (s *SimpleError) String() string {
	return fmt.Sprintf("{ Format=%#v, Arguments=%#v }",
		s.FormatString,
		s.FormatArguments)
}

func (s *SimpleError) Equals(other Node, m EqlMode) bool {
	o, ok := other.(*SimpleError)
	if !ok {
		return false
	}
	return s.FormatString.Equals(o.FormatString, m) &&
		s.FormatArguments.Equals(o.FormatArguments, m)
}

func funSimpleErrorFormatString(ctx context.Context, w *World, s Node) (Node, error) {
	e, err := ExpectClass[*SimpleError](ctx, w, s)
	if err != nil {
		return nil, err
	}
	return e.FormatString, nil
}

func funSimpleErrorFormatArguments(ctx context.Context, w *World, s Node) (Node, error) {
	e, err := ExpectClass[*SimpleError](ctx, w, s)
	if err != nil {
		return nil, err
	}
	return e.FormatArguments, nil
}

func funMakeSimpleError(ctx context.Context, w *World, f, args Node) (Node, error) {
	return &SimpleError{
		FormatString:    f,
		FormatArguments: args,
	}, nil
}
