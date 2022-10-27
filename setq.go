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

		nameNode, params, err = Shift(params)
		if err != nil {
			return nil, err
		}
		nameSymbol, ok := nameNode.(Symbol)
		if !ok {
			return nil, fmt.Errorf("%w: `%s`", ErrExpectedSymbol, ToString(nameSymbol, PRINT))
		}
		value, params, err = w.ShiftAndEvalCar(ctx, params)
		if err != nil {
			return nil, err
		}
		if err := w.Set(nameSymbol, value); err != nil {
			return value, err
		}
	}
	return value, nil
}

func letValuesToVars(ctx context.Context, w *World, list Node, lexical map[Symbol]Node) error {
	for HasValue(list) {
		var item Node
		var err error

		item, list, err = Shift(list)
		if symbol, ok := item.(Symbol); ok {
			lexical[symbol] = Null
			continue
		}
		var argv [2]Node

		if err := ListToArray(item, argv[:]); err != nil {
			return err
		}
		symbol, ok := argv[0].(Symbol)
		if !ok {
			return fmt.Errorf("%w `%s`", ErrExpectedSymbol, ToString(argv[0], PRINT))
		}
		value, err := argv[1].Eval(ctx, w)
		if err != nil {
			return err
		}
		lexical[symbol] = value
	}
	return nil
}

func cmdLet(ctx context.Context, w *World, params Node) (Node, error) {
	// from CommonLisp
	list, params, err := Shift(params)
	if err != nil {
		return nil, err
	}
	lexical := Variables{}

	if err := letValuesToVars(ctx, w, list, lexical); err != nil {
		return nil, err
	}

	newWorld := w.Let(lexical)
	return Progn(ctx, newWorld, params)
}

func cmdLetX(ctx context.Context, w *World, params Node) (Node, error) {
	// from CommonLisp
	list, params, err := Shift(params)
	if err != nil {
		return nil, err
	}
	lexical := Variables{}

	newWorld := w.Let(lexical)

	if err := letValuesToVars(ctx, newWorld, list, lexical); err != nil {
		return nil, err
	}

	return Progn(ctx, newWorld, params)
}

// cmdDefglobal implements (defglobal) of ISLisp and (defparameter) of CommonLisp
func cmdDefglobal(ctx context.Context, w *World, list Node) (Node, error) {
	// from CommonLisp
	var symbolNode Node
	var value Node
	var err error

	symbolNode, list, err = Shift(list)
	if err != nil {
		return nil, err
	}
	symbol, ok := symbolNode.(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}
	value, list, err = w.ShiftAndEvalCar(ctx, list)
	if err != nil {
		return nil, err
	}
	if HasValue(list) {
		return nil, ErrTooManyArguments
	}
	w.DefineGlobal(symbol, value)
	return symbol, nil
}

func cmdDefDynamic(ctx context.Context, w *World, list Node) (Node, error) {
	var err error
	var symbolNode Node

	symbolNode, list, err = Shift(list)
	if err != nil {
		return nil, err
	}
	symbol, ok := symbolNode.(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}
	var value Node
	value, list, err = w.ShiftAndEvalCar(ctx, list)
	if err != nil {
		return nil, err
	}
	if HasValue(list) {
		return nil, ErrTooManyArguments
	}

	w.shared.dynamic.Set(symbol, value)
	return symbol, nil
}

func cmdDynamic(ctx context.Context, w *World, list Node) (Node, error) {
	var err error
	var symbolNode Node

	symbolNode, list, err = Shift(list)
	if err != nil {
		return nil, err
	}
	symbol, ok := symbolNode.(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}
	value, ok := w.shared.dynamic.Get(symbol)
	if !ok {
		return nil, ErrVariableUnbound
	}
	return value, nil
}

func cmdDynamicLet(ctx context.Context, w *World, list Node) (Node, error) {
	var vars Node
	var err error

	vars, list, err = Shift(list)
	if err != nil {
		return nil, err
	}
	backups := make(map[Symbol]Node)
	removes := make(map[Symbol]struct{})
	defer func() {
		for key := range removes {
			delete(w.shared.dynamic, key)
		}
		for key, val := range backups {
			w.shared.dynamic.Set(key, val)
		}
	}()

	for HasValue(vars) {
		var varAndValue Node
		varAndValue, vars, err = Shift(vars)
		if err != nil {
			return nil, err
		}
		if symbol, ok := varAndValue.(Symbol); ok {
			if orig, ok := w.shared.dynamic.Get(symbol); ok {
				backups[symbol] = orig
			} else {
				removes[symbol] = struct{}{}
			}
			w.shared.dynamic.Set(symbol, Null)
		} else {
			var symbolNode Node
			var value Node

			symbolNode, varAndValue, err = Shift(varAndValue)
			if err != nil {
				return nil, err
			}
			symbol, ok := symbolNode.(Symbol)
			if !ok {
				return nil, ErrExpectedSymbol
			}
			value, varAndValue, err = w.ShiftAndEvalCar(ctx, varAndValue)
			if err != nil {
				return nil, err
			}
			if HasValue(varAndValue) {
				return nil, ErrTooManyArguments
			}
			if orig, ok := w.shared.dynamic.Get(symbol); ok {
				backups[symbol] = orig
			} else {
				removes[symbol] = struct{}{}
			}
			w.shared.dynamic.Set(symbol, value)
		}
	}
	return Progn(ctx, w, list)
}
