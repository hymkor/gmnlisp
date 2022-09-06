package common

import (
	"context"

	. "github.com/hymkor/gmnlisp"
)

func cmdDoList(ctx context.Context, w *World, list Node) (Node, error) {
	// from CommonLisp
	var varAndValueNode Node
	var err error

	varAndValueNode, list, err = Shift(list)
	if err != nil {
		return nil, err
	}
	var varAndValues [2]Node
	if err := ListToArray(varAndValueNode, varAndValues[:]); err != nil {
		return nil, err
	}
	symbol, ok := varAndValues[0].(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}
	var last Node = Null
	values, err := varAndValues[1].Eval(ctx, w)
	if err != nil {
		return nil, err
	}
	for HasValue(values) {
		if err := CheckContext(ctx); err != nil {
			return nil, err
		}
		var value1 Node

		value1, values, err = Shift(values)
		if err != nil {
			return nil, err
		}
		if err := w.Set(symbol, value1); err != nil {
			return nil, err
		}
		last, err = Progn(ctx, w, list)
		if err != nil {
			return nil, err
		}
	}
	return last, nil
}

func cmdDoTimes(ctx context.Context, w *World, list Node) (Node, error) {
	// from CommonLisp
	var varAndValueNode Node
	var err error

	varAndValueNode, list, err = Shift(list)
	if err != nil {
		return nil, err
	}
	var varAndValueArray [2]Node
	if err := ListToArray(varAndValueNode, varAndValueArray[:]); err != nil {
		return nil, err
	}
	symbol, ok := varAndValueArray[0].(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}
	endNode, err := varAndValueArray[1].Eval(ctx, w)
	if err != nil {
		return nil, err
	}
	end, ok := endNode.(Integer)
	if !ok {
		return nil, ErrExpectedNumber
	}

	var last Node = Null
	for i := Integer(0); i < end; i++ {
		if err := CheckContext(ctx); err != nil {
			return nil, err
		}
		if err := w.Set(symbol, i); err != nil {
			return nil, err
		}
		last, err = Progn(ctx, w, list)
		if err != nil {
			return nil, err
		}
	}
	return last, nil
}
