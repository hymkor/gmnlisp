package gmnlisp

import (
	"context"
	"fmt"
)

func cmdSetq(ctx context.Context, w *World, params Node) (Node, error) {
	var value Node = Null

	for HasValue(params) {
		var nameNode Node
		var err error

		nameNode, params, err = shift(params)
		if err != nil {
			return nil, err
		}
		nameSymbol, ok := nameNode.(Symbol)
		if !ok {
			return nil, fmt.Errorf("%w: `%s`", ErrExpectedSymbol, toString(nameSymbol))
		}
		value, params, err = w.shiftAndEvalCar(ctx, params)
		if err != nil {
			return nil, err
		}
		w.SetOrNew(string(nameSymbol), value)
	}
	return value, nil
}

func cmdLet(ctx context.Context, w *World, params Node) (Node, error) {
	list, params, err := shift(params)
	if err != nil {
		return nil, err
	}
	globals := map[string]Node{}

	for HasValue(list) {
		var item Node

		item, list, err = shift(list)
		if symbol, ok := item.(Symbol); ok {
			globals[string(symbol)] = Null
			continue
		}
		var argv [2]Node

		if err := listToArray(item, argv[:]); err != nil {
			return nil, err
		}
		symbol, ok := argv[0].(Symbol)
		if !ok {
			return nil, fmt.Errorf("%w `%s`", ErrExpectedSymbol, toString(argv[0]))
		}
		value, err := argv[1].Eval(ctx, w)
		if err != nil {
			return nil, err
		}
		globals[string(symbol)] = value
	}

	newWorld := &World{
		globals: globals,
		parent:  w,
	}
	return progn(ctx, newWorld, params)
}

func cmdDefvar(ctx context.Context, w *World, list Node) (Node, error) {
	var symbolNode Node
	var err error

	symbolNode, list, err = shift(list)
	if err != nil {
		return nil, err
	}
	symbol, ok := symbolNode.(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}
	var value Node = Null
	if HasValue(list) {
		value, list, err = w.shiftAndEvalCar(ctx, list)
		if err != nil {
			return nil, err
		}
	}
	for w.parent != nil {
		w = w.parent
	}
	w.globals[string(symbol)] = value
	return value, nil
}
