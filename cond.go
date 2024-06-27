package gmnlisp

import (
	"context"
	"errors"
	"fmt"
)

func cmdWithHandler(ctx context.Context, w *World, node Node) (Node, error) {
	_handler, node, err := w.ShiftAndEvalCar(ctx, node)
	if err != nil {
		return nil, err
	}
	handler, err := ExpectFunction(_handler)
	if err != nil {
		return nil, err
	}
	oldHandler := w.handler
	defer func() { w.handler = oldHandler }()
	w.handler = handler
	value, err := Progn(ctx, w, node)

	// 本来、場合によっては継続処理があるため、ハンドラー関数はエラーが起きた場所で
	// 呼ばなければいけないが、 古いコードは error を返して、error の処理元で
	// ハンドラー関数を呼ぶことを想定し、エラー発生箇所で呼べていない
	//
	// エラー発生箇所でハンドラー関数が呼ばれて、ここに戻る時は大域脱出系エラーで
	// 戻ってくるはずなので、そういった場合は、ここでハンドラー関数を呼ばない
	// ようにしている
	// (将来的にはエラーが起きた場所で呼ぶよう変更しなくてはいけない)

	var e1 *_ErrEarlyReturns // block & return-from
	var e2 *_ErrThrown       // catch & throw
	var e3 *_ErrTagBody      // tagbody & go
	if err == nil || errors.As(err, &e1) || errors.As(err, &e2) || errors.As(err, &e3) {
		return value, err
	}

	var errorValue interface {
		Node
		Error() string
	}
	var err2 error
	if errors.As(err, &errorValue) {
		_, err2 = handler.Call(ctx, w, &Cons{Car: errorValue, Cdr: Null})
	} else {
		_, err2 = handler.Call(ctx, w, &Cons{Car: &ErrorNode{Value: err}, Cdr: Null})
	}
	if err2 != nil {
		return nil, err2
	}
	return nil, err
}

type _ErrContinueCondition struct {
	Cond  Node
	Value Node
}

func (e *_ErrContinueCondition) Error() string {
	return e.Value.String()
}

var symReportCondition = NewSymbol("report-condition")
var reportCondition = &_Generic{
	Symbol:  symReportCondition,
	argc:    2,
	methods: []*_Method{},
}

func funSignalCondition(ctx context.Context, w *World, args []Node) (Node, error) {
	cond := args[0]
	continueable := args[1]
	if w.handler == nil {
		buffer := &StringBuilder{}
		if _, err := reportCondition.Call(ctx, w, &Cons{Car: Uneval{Node: cond}, Cdr: &Cons{Car: Uneval{Node: buffer}}}); err == nil {
			return nil, errors.New(buffer.String())
		} else if !errors.Is(err, ErrNoMatchMethods) {
			return nil, fmt.Errorf("%w in (report-condition)", err)
		}
		if err, ok := args[0].(error); ok {
			return nil, err
		}
		return nil, errors.New(args[0].String())
	}
	rv, err := w.handler.Call(ctx, w, &Cons{Car: cond})
	var e *_ErrContinueCondition
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
	return nil, &_ErrContinueCondition{Cond: cond, Value: value}
}
