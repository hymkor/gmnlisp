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
	value, err := Progn(ctx, w, node)

	// 本来、場合によっては継続処理があるため、ハンドラー関数はエラーが起きた場所で
	// 呼ばなければいけないが、 古いコードは error を返して、error の処理元で
	// ハンドラー関数を呼ぶことを想定し、エラー発生箇所で呼べていない
	//
	// エラー発生箇所でハンドラー関数が呼ばれて、ここに戻る時は大域脱出系エラーで
	// 戻ってくるはずなので、そういった場合は、ここでハンドラー関数を呼ばない
	// ようにしている
	// (将来的にはエラーが起きた場所で呼ぶよう変更しなくてはいけない)

	var e1 *ErrEarlyReturns // block & return-from
	var e2 *ErrThrown       // catch & throw
	var e3 *_TagBody        // tagbody & go
	if err == nil || errors.As(err, &e1) || errors.As(err, &e2) || errors.As(err, &e3) {
		return value, err
	}

	_, err2 := handler.Call(ctx, w, &Cons{Car: &ErrorNode{Value: err}, Cdr: Null})
	if err2 != nil {
		return nil, err2
	}
	return nil, err
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
