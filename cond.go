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

	// 本来、場合によっては継続処理があるため、ハンドラー関数はエラーが起きた場所で
	// 呼ばなければいけないが、 古いコードは error を返して、error の処理元で
	// ハンドラー関数を呼ぶことを想定し、エラー発生箇所で呼べていない
	//
	// エラー発生箇所でハンドラー関数が呼ばれて、ここに戻る時は大域脱出系エラーで
	// 戻ってくるはずなので、そういった場合は、ここでハンドラー関数を呼ばない
	// ようにしている
	// (将来的にはエラーが起きた場所で呼ぶよう変更しなくてはいけない)

	if err == nil || IsNonLocalExists(err) || errors.Is(err, errHandlerReturnNormally) {
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
		_, err2 = handler.Call(ctx, w, &Cons{Car: ErrorNode{Value: err}, Cdr: Null})
	}
	if err2 != nil {
		return nil, err2
	}
	return raiseControlError(ctx, w, errors.New("Handler return normally"))
}

type _ErrContinueCondition struct {
	Cond  Node
	Value Node
}

func (e *_ErrContinueCondition) Error() string {
	return "continue-condition failed for " + e.Value.String()
}

var symReportCondition = NewSymbol("report-condition")
var reportCondition = &_Generic{
	Symbol:  symReportCondition,
	argc:    2,
	methods: []*_Method{},
}

func funSignalCondition(ctx context.Context, w *World, cond, continueable Node) (Node, error) {
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
	rv, err := w.handler[len(w.handler)-1].Call(ctx, w, &Cons{Car: cond})
	var e *_ErrContinueCondition
	if IsSome(continueable) && errors.As(err, &e) {
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
