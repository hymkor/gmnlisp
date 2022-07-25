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
		w.Set(string(nameSymbol), value)
	}
	return value, nil
}

func cmdLet(ctx context.Context, w *World, param Node) (Node, error) {
	cons, ok := param.(*Cons)
	if !ok {
		return nil, fmt.Errorf("%w: `%s`", ErrExpectedCons, toString(param))
	}
	code := cons.Cdr

	globals := map[string]Node{}

	err := forEachList(cons.Car, func(node Node) error {
		if symbol, ok := node.(Symbol); ok {
			globals[string(symbol)] = Null
			return nil
		}
		var argv [2]Node

		if err := listToArray(node, argv[:]); err != nil {
			return err
		}
		symbol, ok := argv[0].(Symbol)
		if !ok {
			return fmt.Errorf("%w `%s`", ErrExpectedSymbol, toString(argv[0]))
		}
		value, err := argv[1].Eval(ctx, w)
		if err != nil {
			return err
		}
		globals[string(symbol)] = value
		return nil
	})
	if err != nil {
		return nil, err
	}

	newWorld := &World{
		globals: globals,
		parent:  w,
	}
	return progn(ctx, newWorld, code)
}
