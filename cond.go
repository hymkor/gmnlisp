package gmnlisp

import (
	"context"
	"errors"
	"fmt"
)

func IsNonLocalExists(err error) bool {
	var e1 *_ErrEarlyReturns // block & return-from
	var e2 *_ErrThrown       // catch & throw
	var e3 *_ErrTagBody      // tagbody & go

	return errors.As(err, &e1) || errors.As(err, &e2) || errors.As(err, &e3)
}

func cmdWithHandler(ctx context.Context, w *World, node Node) (Node, error) {
	_handler, node, err := w.ShiftAndEvalCar(ctx, node)
	if err != nil {
		return nil, err
	}
	handler, err := ExpectFunction(ctx, w, _handler)
	if err != nil {
		return nil, err
	}
	w.handler = append(w.handler, handler)

	value, err := Progn(ctx, w, node)

	if L := len(w.handler); L > 0 {
		w.handler = w.handler[:L-1]
	}

	// For error conditions where continuation is not required,
	// the handler function is invoked here.
	//
	// If the handler has already been invoked at the error site and
	// returned with no continuation, it is not called again.
	//
	// Non-local exit information may also be returned as an error value,
	// but since this is not a Lisp error, it is simply returned to the caller.

	if err == nil || IsNonLocalExists(err) || errors.Is(err, errHandlerReturnNormally) {
		return value, err
	}
	var errorValue Condition
	var err2 error
	if errors.As(err, &errorValue) {
		_, err2 = handler.Call(ctx, w, &Cons{Car: errorValue, Cdr: Null})
	} else {
		_, err2 = handler.Call(ctx, w, &Cons{Car: ErrorNode{Value: err}, Cdr: Null})
	}
	if err2 != nil {
		return nil, err2
	}
	return raiseControlError(ctx, w, errors.New("handler return normally"))
}

type _ErrContinueCondition struct {
	Cond  Node
	Value Node
}

func (e *_ErrContinueCondition) Error() string {
	return "continue-condition failed for " + e.Value.String()
}

var symReportCondition = NewSymbol("report-condition")
var reportCondition = &genericType{
	Symbol:  symReportCondition,
	argc:    2,
	methods: []*methodType{},
}

type Continuable interface {
	SetContinuableString(String)
	ContinuableString() Node
}

func funSignalCondition(ctx context.Context, w *World, cond, continuable Node) (Node, error) {
	if !errorClass.InstanceP(cond) {
		return nil, &DomainError{
			Object:        cond,
			ExpectedClass: errorClass,
		}
	}
	if len(w.handler) <= 0 {
		buffer := &StringBuilder{}
		if _, err := reportCondition.Call(ctx, w, UnevalList(cond, buffer)); err == nil {
			return nil, errors.New(buffer.String())
		} else if !errors.Is(err, ErrNoMatchMethods) {
			return nil, fmt.Errorf("%w in (report-condition)", err)
		}
		if err, ok := cond.(error); ok {
			return nil, err
		}
		return nil, errors.New(cond.String())
	}
	if c, ok := cond.(Continuable); ok {
		if s, ok := continuable.(String); ok {
			c.SetContinuableString(s)
		} else if True.Equals(continuable, STRICT) {
			c.SetContinuableString("Continue with no special action.")
		} else if IsSome(continuable) {
			return nil, &DomainError{
				Object:        continuable,
				ExpectedClass: stringClass,
			}
		}
	}
	rv, err := w.handler[len(w.handler)-1].Call(ctx, w, &Cons{Car: cond})
	var e *_ErrContinueCondition
	if IsSome(continuable) && errors.As(err, &e) {
		return e.Value, nil
	}
	return rv, err
}

func funContinueCondition(ctx context.Context, w *World, args []Node) (Node, error) {
	_, ok := args[0].(error)
	if !ok {
		var err error
		args[0], err = callHandler[Node](ctx, w, false, &DomainError{
			Object:        args[0],
			ExpectedClass: errorClass,
		})
		if err != nil {
			return args[0], err
		}
	}
	e := &_ErrContinueCondition{Cond: args[0]}
	if len(args) >= 2 {
		e.Value = args[1]
	} else {
		e.Value = Null
	}
	return nil, e
}

func funConditionContinuable(ctx context.Context, w *World, arg Node) (Node, error) {
	e, err := ExpectInterface[Condition](ctx, w, arg, errorClass)
	if err != nil {
		return nil, err
	}
	var ce Continuable
	if !errors.As(e, &ce) {
		return Null, nil
	}
	return ce.ContinuableString(), nil
}
