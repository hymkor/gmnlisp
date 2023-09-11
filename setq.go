package gmnlisp

import (
	"context"
)

func cmdSetq(ctx context.Context, w *World, params Node) (Node, error) {
	var value Node = Null

	for IsSome(params) {
		var nameNode Node
		var err error

		nameNode, params, err = Shift(params)
		if err != nil {
			return nil, err
		}
		nameSymbol, ok := nameNode.(Symbol)
		if !ok {
			return nil, MakeError(ErrExpectedSymbol, nameSymbol)
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

func cmdPSetq(ctx context.Context, w *World, params Node) (Node, error) {
	type assignT struct {
		symbol Symbol
		value  Node
	}
	assignList := []*assignT{}

	for IsSome(params) {
		var nameNode Node
		var err error
		var value Node

		nameNode, params, err = Shift(params)
		if err != nil {
			return nil, err
		}
		nameSymbol, ok := nameNode.(Symbol)
		if !ok {
			return nil, MakeError(ErrExpectedSymbol, nameSymbol)
		}
		value, params, err = w.ShiftAndEvalCar(ctx, params)
		if err != nil {
			return nil, err
		}
		assignList = append(assignList, &assignT{symbol: nameSymbol, value: value})
	}
	for _, a := range assignList {
		if err := w.Set(a.symbol, a.value); err != nil {
			return nil, err
		}
	}
	return Null, nil
}

func letValuesToVars(ctx context.Context, w *World, list Node, lexical map[Symbol]Node) error {
	for IsSome(list) {
		var item Node
		var err error

		item, list, err = Shift(list)
		if err != nil {
			return err
		}
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
			return MakeError(ErrExpectedSymbol, argv[0])
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
	return cmdLetWithTailRecOpt(ctx, w, params, -1)
}

func cmdLetWithTailRecOpt(ctx context.Context, w *World, params Node, tailOptSym Symbol) (Node, error) {
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
	return prognWithTailRecOpt(ctx, newWorld, params, tailOptSym)
}

func cmdLetX(ctx context.Context, w *World, params Node) (Node, error) {
	return cmdLetXWithTailRecOpt(ctx, w, params, -1)
}

func cmdLetXWithTailRecOpt(ctx context.Context, w *World, params Node, tailOptSym Symbol) (Node, error) {
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
	return prognWithTailRecOpt(ctx, newWorld, params, tailOptSym)
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
	if IsSome(list) {
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
	if IsSome(list) {
		return nil, ErrTooManyArguments
	}

	w.shared.dynamic.Set(symbol, value)
	return symbol, nil
}

func cmdDynamic(ctx context.Context, w *World, list Node) (Node, error) {
	var err error
	var symbolNode Node

	symbolNode, _, err = Shift(list)
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

type Dynamics struct {
	backups map[Symbol]Node
	removes map[Symbol]struct{}
	world   *World
}

func (D *Dynamics) Close() {
	for key := range D.removes {
		delete(D.world.shared.dynamic, key)
	}
	for key, val := range D.backups {
		D.world.shared.dynamic.Set(key, val)
	}
}

func (w *World) NewDynamics() *Dynamics {
	return &Dynamics{
		backups: make(map[Symbol]Node),
		removes: make(map[Symbol]struct{}),
		world:   w,
	}
}

func (D *Dynamics) Add(symbol Symbol, newValue Node) {
	if orig, ok := D.world.shared.dynamic.Get(symbol); ok {
		D.backups[symbol] = orig
	} else {
		D.removes[symbol] = struct{}{}
	}
	if newValue != nil {
		D.world.shared.dynamic.Set(symbol, newValue)
	}
}

func cmdDynamicLet(ctx context.Context, w *World, list Node) (Node, error) {
	var vars Node
	var err error

	D := w.NewDynamics()
	defer D.Close()

	vars, list, err = Shift(list)
	if err != nil {
		return nil, err
	}

	for IsSome(vars) {
		var varAndValue Node
		varAndValue, vars, err = Shift(vars)
		if err != nil {
			return nil, err
		}
		if symbol, ok := varAndValue.(Symbol); ok {
			D.Add(symbol, Null)
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
			if IsSome(varAndValue) {
				return nil, ErrTooManyArguments
			}
			D.Add(symbol, value)
		}
	}
	return Progn(ctx, w, list)
}
