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

type LeftValueF struct {
	C int
	F func(context.Context, *World, []Node) (Node, func(Node) error, error)
}

func (*LeftValueF) PrintTo(w io.Writer, m PrintMode) (int, error) {
	return io.WriteString(w, "buildin function(Set/Get)")
}

func (f *LeftValueF) Eval(context.Context, *World) (Node, error) {
	return f, nil
}

func (f *LeftValueF) Equals(n Node, m EqlMode) bool {
	return false
}

func (f *LeftValueF) Call(ctx context.Context, w *World, list Node) (Node, error) {
	var argv [maxParameterOfEasyFunc]Node
	var err error

	if err := checkContext(ctx); err != nil {
		return nil, err
	}

	if f.C >= 0 {
		for i := 0; i < f.C; i++ {
			argv[i], list, err = w.shiftAndEvalCar(ctx, list)
			if err != nil {
				return nil, err
			}
		}
		if HasValue(list) {
			return nil, ErrTooManyArguments
		}
		value, _, err := f.F(ctx, w, argv[:f.C])
		return value, err
	} else {
		args := []Node{}
		for HasValue(list) {
			var arg1 Node
			arg1, list, err = w.shiftAndEvalCar(ctx, list)
			if err != nil {
				return nil, err
			}
			args = append(args, arg1)
		}
		value, _, err := f.F(ctx, w, args)
		return value, err
	}
}

func (f *LeftValueF) Set(ctx context.Context, w *World, list Node, value Node) error {
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
	if f.C >= 0 {
		if len(args) < f.C {
			return ErrTooFewArguments
		}
		if len(args) > f.C {
			return ErrTooManyArguments
		}
	}
	_, setter, err := f.F(ctx, w, args)
	if err != nil {
		return err
	}
	return setter(value)
}

func cmdSetf(ctx context.Context, w *World, params Node) (Node, error) {
	type hasSetter interface {
		Set(ctx context.Context, w *World, list Node, value Node) error
	}

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
			f, ok := _f.(hasSetter)
			if !ok {
				return nil, fmt.Errorf("%w: %s", ErrNotSupportBySetf, commandName)
			}
			return rightValue, f.Set(ctx, w, list, rightValue)
		} else {
			return nil, fmt.Errorf("%w: %s", ErrExpectedSymbolOrList, toString(leftValue, PRINT))
		}
	}
	return value, nil
}

func letValuesToVars(ctx context.Context, w *World, list Node, lexical map[Symbol]Node) error {
	for HasValue(list) {
		var item Node
		var err error

		item, list, err = shift(list)
		if symbol, ok := item.(Symbol); ok {
			lexical[symbol] = Null
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
		lexical[symbol] = value
	}
	return nil
}

func cmdLet(ctx context.Context, w *World, params Node) (Node, error) {
	// from CommonLisp
	list, params, err := shift(params)
	if err != nil {
		return nil, err
	}
	lexical := map[Symbol]Node{}

	if err := letValuesToVars(ctx, w, list, lexical); err != nil {
		return nil, err
	}

	newWorld := &World{
		lexical: _Variables(lexical),
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
	lexical := map[Symbol]Node{}

	newWorld := &World{
		lexical: _Variables(lexical),
		parent:  w,
	}

	if err := letValuesToVars(ctx, newWorld, list, lexical); err != nil {
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
