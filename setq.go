package gmnlisp

import (
	"context"
	"fmt"
	"io"
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

type SetGetF struct {
	C int
	F func(context.Context, *World, []Node) (*Node, error)
}

func (*SetGetF) PrintTo(w io.Writer, m PrintMode) {
	io.WriteString(w, "buildin function(Set/Get)")
}

func (f *SetGetF) Eval(context.Context, *World) (Node, error) {
	return f, nil
}

func (f *SetGetF) Equals(n Node, m EqlMode) bool {
	return false
}

func (f *SetGetF) Call(ctx context.Context, w *World, list Node) (Node, error) {
	var argv [maxParameterOfEasyFunc]Node
	var err error

	if err := checkContext(ctx); err != nil {
		return nil, err
	}

	for i := 0; i < f.C; i++ {
		argv[i], list, err = w.shiftAndEvalCar(ctx, list)
		if err != nil {
			return nil, err
		}
	}
	if HasValue(list) {
		return nil, ErrTooManyArguments
	}
	ptr, err := f.F(ctx, w, argv[:f.C])
	if err != nil {
		return nil, err
	}
	if ptr == nil || *ptr == nil {
		return Null, nil
	}
	return *ptr, nil
}

func (f *SetGetF) Set(ctx context.Context, w *World, list Node, value Node) error {
	args := []Node{}
	for HasValue(list) {
		var tmp Node
		var err error

		tmp, list, err = w.shiftAndEvalCar(ctx, list)
		if err != nil {
			return err
		}
		args = append(args, tmp)
	}
	if len(args) < f.C {
		return ErrTooFewArguments
	}
	if len(args) > f.C {
		return ErrTooManyArguments
	}
	ptr, err := f.F(ctx, w, args)
	if err != nil {
		return err
	}
	*ptr = value
	return nil
}

func cmdSetf(ctx context.Context, w *World, params Node) (Node, error) {
	var value Node = Null

	for HasValue(params) {
		var leftValue Node
		var rightValue Node
		var err error

		leftValue, params, err = shift(params)
		if err != nil {
			return nil, err
		}
		rightValue, params, err = w.shiftAndEvalCar(ctx, params)
		if err != nil {
			return nil, err
		}

		if nameSymbol, ok := leftValue.(Symbol); ok {
			if err := w.Set(nameSymbol, rightValue); err != nil {
				return value, err
			}
		} else if list, ok := leftValue.(*Cons); ok {
			commandName, list, err := shift(list)
			if err != nil {
				return nil, err
			}
			commandSymbol, ok := commandName.(Symbol)
			if !ok {
				return nil, ErrExpectedSymbol
			}
			_f, err := w.Get(commandSymbol)
			if err != nil {
				return nil, ErrVariableUnbound
			}
			f, ok := _f.(*SetGetF)
			if !ok {
				return nil, ErrVariableUnbound
			}
			return rightValue, f.Set(ctx, w, list, rightValue)
		} else {
			return nil, fmt.Errorf("%w: `%s`", ErrExpectedSymbol, toString(nameSymbol, PRINT))
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
