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
			return nil, fmt.Errorf("%w: `%s`", ErrExpectedSymbol, toString(nameSymbol, PRINT))
		}
		value, params, err = w.shiftAndEvalCar(ctx, params)
		if err != nil {
			return nil, err
		}
		if err := w.Set(nameSymbol, value); err != nil {
			return value, err
		}
	}
	return value, nil
}

func letValuesToVars(ctx context.Context, w *World, list Node, globals map[Symbol]Node) error {
	for HasValue(list) {
		var item Node
		var err error

		item, list, err = shift(list)
		if symbol, ok := item.(Symbol); ok {
			globals[symbol] = Null
			continue
		}
		var argv [2]Node

		if err := listToArray(item, argv[:]); err != nil {
			return err
		}
		symbol, ok := argv[0].(Symbol)
		if !ok {
			return fmt.Errorf("%w `%s`", ErrExpectedSymbol, toString(argv[0], PRINT))
		}
		value, err := argv[1].Eval(ctx, w)
		if err != nil {
			return err
		}
		globals[symbol] = value
	}
	return nil
}

func cmdLet(ctx context.Context, w *World, params Node) (Node, error) {
	// from CommonLisp
	list, params, err := shift(params)
	if err != nil {
		return nil, err
	}
	globals := map[Symbol]Node{}

	if err := letValuesToVars(ctx, w, list, globals); err != nil {
		return nil, err
	}

	newWorld := &World{
		globals: globals,
		parent:  w,
	}
	return progn(ctx, newWorld, params)
}

func cmdLetX(ctx context.Context, w *World, params Node) (Node, error) {
	// from CommonLisp
	list, params, err := shift(params)
	if err != nil {
		return nil, err
	}
	globals := map[Symbol]Node{}

	newWorld := &World{
		globals: globals,
		parent:  w,
	}

	if err := letValuesToVars(ctx, newWorld, list, globals); err != nil {
		return nil, err
	}

	return progn(ctx, newWorld, params)
}

func cmdDefvar(ctx context.Context, w *World, list Node) (Node, error) {
	// from CommonLisp
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
	w.DefineVariable(symbol, func() Node {
		var value Node = Null
		if HasValue(list) {
			value, _, err = w.shiftAndEvalCar(ctx, list)
		}
		return value
	})
	return symbol, err
}

func cmdDefparameter(ctx context.Context, w *World, list Node) (Node, error) {
	// from CommonLisp
	var symbolNode Node
	var value Node
	var err error

	symbolNode, list, err = shift(list)
	if err != nil {
		return nil, err
	}
	symbol, ok := symbolNode.(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}
	value, list, err = w.shiftAndEvalCar(ctx, list)
	if err != nil {
		return nil, err
	}
	if HasValue(list) {
		return nil, ErrTooManyArguments
	}
	w.DefineParameter(symbol, value)
	return symbol, nil
}
