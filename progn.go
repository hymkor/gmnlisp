package gmnlisp

import (
	"context"
	"errors"
	"fmt"
	"io"
)

type ErrEarlyReturns struct {
	Value Node
	Name  string
}

func (e *ErrEarlyReturns) Error() string {
	if e.Name == "" {
		return "Unexpected (return)"
	}
	return fmt.Sprintf("Unexpected (return-from %s)", e.Name)
}

func cmdReturn(ctx context.Context, w *World, n Node) (Node, error) {
	var argv [1]Node
	if err := w.evalListAll(ctx, n, argv[:]); err != nil {
		return nil, err
	}
	return nil, &ErrEarlyReturns{Value: argv[0], Name: ""}
}

func cmdReturnFrom(ctx context.Context, w *World, n Node) (Node, error) {
	var argv [2]Node
	if err := listToArray(n, argv[:]); err != nil {
		return nil, err
	}
	symbol, ok := argv[0].(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}
	value, err := argv[1].Eval(ctx, w)
	if err != nil {
		return nil, err
	}
	return nil, &ErrEarlyReturns{Value: value, Name: string(symbol)}
}

func progn(ctx context.Context, w *World, n Node) (value Node, err error) {
	for HasValue(n) {
		value, n, err = w.shiftAndEvalCar(ctx, n)
		if err != nil {
			return nil, err
		}
	}
	return
}

func cmdProgn(ctx context.Context, w *World, c Node) (Node, error) {
	return progn(ctx, w, c)
}

func cmdBlock(ctx context.Context, w *World, node Node) (Node, error) {
	cons, ok := node.(*Cons)
	if !ok {
		return nil, ErrExpectedCons
	}
	_name, ok := cons.Car.(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}
	name := string(_name)

	var errEarlyReturns *ErrEarlyReturns
	rv, err := progn(ctx, w, cons.Cdr)
	if errors.As(err, &errEarlyReturns) && errEarlyReturns.Name == name {
		return errEarlyReturns.Value, nil
	}
	return rv, err
}

func cmdCond(ctx context.Context, w *World, list Node) (Node, error) {
	var last Node
	err := forEachList(list, func(condAndAct Node) error {
		cond, act, err := w.shiftAndEvalCar(ctx, condAndAct)
		if err != nil {
			return err
		}
		if IsNull(cond) {
			return nil
		}
		last, err = progn(ctx, w, act)
		if err == nil {
			err = io.EOF
		}
		return err
	})
	if err == io.EOF {
		err = nil
	}
	return last, err
}

func cmdIf(ctx context.Context, w *World, param Node) (Node, error) {
	argv, err := listToSlice(param)
	if err != nil {
		return nil, err
	}
	if len(argv) > 3 {
		return nil, ErrTooManyArguments
	}
	if len(argv) < 2 {
		return nil, ErrTooFewArguments
	}
	cond, err := argv[0].Eval(ctx, w)
	if err != nil {
		return nil, err
	}
	if HasValue(cond) {
		return argv[1].Eval(ctx, w)
	} else if len(argv) == 3 {
		return argv[2].Eval(ctx, w)
	}
	return Null, nil
}

func cmdForeach(ctx context.Context, w *World, n Node) (Node, error) {
	cons, ok := n.(*Cons)
	if !ok {
		return nil, fmt.Errorf("(1): %w", ErrExpectedCons)
	}
	symbol, ok := cons.Car.(Symbol)
	if !ok {
		return nil, fmt.Errorf("(1): %w", ErrExpectedSymbol)
	}

	list, code, err := w.shiftAndEvalCar(ctx, cons.Cdr)
	if err != nil {
		return nil, err
	}

	var last Node
	for HasValue(list) {
		var err error

		cons, ok := list.(*Cons)
		if !ok {
			return nil, ErrExpectedCons
		}
		w.Set(string(symbol), cons.Car)
		list = cons.Cdr

		last, err = progn(ctx, w, code)
		if err != nil {
			return nil, err
		}
	}
	return last, nil
}

func cmdWhile(ctx context.Context, w *World, n Node) (Node, error) {
	cons, ok := n.(*Cons)
	if !ok {
		return nil, ErrTooFewArguments
	}
	cond := cons.Car
	statements := cons.Cdr
	var last Node = Null
	for {
		cont, err := cond.Eval(ctx, w)
		if err != nil {
			return nil, err
		}
		if IsNull(cont) {
			return last, nil
		}
		last, err = progn(ctx, w, statements)
		if err != nil {
			return nil, err
		}
	}
}

func cmdQuit(context.Context, *World, Node) (Node, error) {
	return Null, ErrQuit
}
