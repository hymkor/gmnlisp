package gmnlisp

import (
	"errors"
	"fmt"
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

func cmdReturn(w *World, n Node) (Node, error) {
	var argv [1]Node
	if err := w.evalListAll(n, argv[:]); err != nil {
		return nil, err
	}
	return nil, &ErrEarlyReturns{Value: argv[0], Name: ""}
}

func cmdReturnFrom(w *World, n Node) (Node, error) {
	var argv [2]Node
	if err := listToArray(n, argv[:]); err != nil {
		return nil, err
	}
	symbol, ok := argv[0].(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}
	value, err := argv[1].Eval(w)
	if err != nil {
		return nil, err
	}
	return nil, &ErrEarlyReturns{Value: value, Name: string(symbol)}
}

func progn(w *World, n Node) (value Node, err error) {
	for HasValue(n) {
		value, n, err = w.shiftAndEvalCar(n)
		if err != nil {
			return nil, err
		}
	}
	return
}

func cmdProgn(w *World, c Node) (Node, error) {
	return progn(w, c)
}

func cmdBlock(w *World, node Node) (Node, error) {
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
	rv, err := progn(w, cons.Cdr)
	if errors.As(err, &errEarlyReturns) && errEarlyReturns.Name == name {
		return errEarlyReturns.Value, nil
	}
	return rv, err
}

func cmdCond(w *World, node Node) (Node, error) {
	for HasValue(node) {
		cons, ok := node.(*Cons)
		if !ok {
			return nil, ErrExpectedCons
		}
		node = cons.Cdr

		conditionAndActions, ok := cons.Car.(*Cons)
		if !ok {
			return nil, fmt.Errorf("%w: %s", ErrExpectedCons, toString(cons.Car))
		}
		condition, err := conditionAndActions.GetCar().Eval(w)
		if err != nil {
			return nil, err
		}
		if HasValue(condition) {
			result, err := progn(w, conditionAndActions.Cdr)
			if err != nil {
				return result, err
			}
			return result, err
		}
	}
	return Null, nil
}

func cmdIf(w *World, param Node) (Node, error) {
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
	cond, err := argv[0].Eval(w)
	if err != nil {
		return nil, err
	}
	if HasValue(cond) {
		return argv[1].Eval(w)
	} else if len(argv) == 3 {
		return argv[2].Eval(w)
	}
	return Null, nil
}

func cmdForeach(w *World, n Node) (Node, error) {
	cons, ok := n.(*Cons)
	if !ok {
		return nil, fmt.Errorf("(1): %w", ErrExpectedCons)
	}
	symbol, ok := cons.Car.(Symbol)
	if !ok {
		return nil, fmt.Errorf("(1): %w", ErrExpectedSymbol)
	}

	list, code, err := w.shiftAndEvalCar(cons.Cdr)
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

		last, err = progn(w, code)
		if err != nil {
			return nil, err
		}
	}
	return last, nil
}

func cmdWhile(w *World, n Node) (Node, error) {
	cons, ok := n.(*Cons)
	if !ok {
		return nil, ErrTooFewArguments
	}
	cond := cons.Car
	statements := cons.Cdr
	var last Node = Null
	for {
		cont, err := cond.Eval(w)
		if err != nil {
			return nil, err
		}
		if IsNull(cont) {
			return last, nil
		}
		last, err = progn(w, statements)
		if err != nil {
			return nil, err
		}
	}
}

func cmdQuit(*World, Node) (Node, error) {
	return Null, ErrQuit
}
