package gmnlisp

import (
	"context"
	"errors"
	"fmt"
)

var activeHandleFuncKey = genSym()

func cmdWithHandler(ctx context.Context, w *World, node Node) (Node, error) {
	_handler, node, err := w.ShiftAndEvalCar(ctx, node)
	if err != nil {
		return nil, err
	}
	handler, ok := _handler.(Callable)
	if !ok {
		return nil, ErrExpectedFunction
	}
	D := w.NewDynamics()
	defer D.Close()
	D.Set(activeHandleFuncKey, handler)
	return Progn(ctx, w, node)
}

type ErrContinueCondition struct {
	Cond  Node
	Value Node
}

func (e *ErrContinueCondition) Error() string {
	return e.Value.String()
}

func funSignalCondition(ctx context.Context, w *World, args []Node) (Node, error) {
	cond := args[0]
	continueable := args[1]
	_handler := w.Dynamic(activeHandleFuncKey)
	if IsNull(_handler) {
		return nil, fmt.Errorf("no active handler(%s) for %v", activeHandleFuncKey, cond)
	}
	handler, ok := _handler.(Callable)
	if !ok {
		panic("handler is not callable")
	}
	rv, err := handler.Call(ctx, w, &Cons{Car: cond})
	var e *ErrContinueCondition
	if IsSome(continueable) && errors.As(err, &e) {
		return e.Value, nil
	}
	return rv, err
}

func cmdContinueCondition(ctx context.Context, w *World, node Node) (Node, error) {
	cond, node, err := w.ShiftAndEvalCar(ctx, node)
	if err != nil {
		return nil, err
	}
	value, node, err := w.ShiftAndEvalCar(ctx, node)
	if err != nil {
		return nil, err
	}
	if IsSome(node) {
		return nil, ErrTooManyArguments
	}
	return nil, &ErrContinueCondition{Cond: cond, Value: value}
}
