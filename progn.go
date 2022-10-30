package gmnlisp

import (
	"context"
	"errors"
	"fmt"
	"io"
)

type ErrEarlyReturns struct {
	Value Node
	Name  Symbol
}

func (e *ErrEarlyReturns) Error() string {
	if e.Name == nulSymbol {
		return "Unexpected (return)"
	}
	return fmt.Sprintf("Unexpected (return-from %s)", e.Name)
}

func funReturn(ctx context.Context, w *World, argv []Node) (Node, error) {
	// from CommonLisp
	return nil, &ErrEarlyReturns{Value: argv[0], Name: nulSymbol}
}

func cmdReturnFrom(ctx context.Context, w *World, n Node) (Node, error) {
	// from CommonLisp
	var argv [2]Node
	if err := ListToArray(n, argv[:]); err != nil {
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
	return nil, &ErrEarlyReturns{Value: value, Name: symbol}
}

func Progn(ctx context.Context, w *World, n Node) (value Node, err error) {
	value = Null
	for HasValue(n) {
		value, n, err = w.ShiftAndEvalCar(ctx, n)
		if err != nil {
			return nil, err
		}
	}
	return
}

func cmdProgn(ctx context.Context, w *World, c Node) (Node, error) {
	return Progn(ctx, w, c)
}

func cmdBlock(ctx context.Context, w *World, node Node) (Node, error) {
	// from CommonLisp
	nameNode, statements, err := Shift(node)
	if err != nil {
		return nil, err
	}
	var nameSymbol Symbol

	if HasValue(nameNode) {
		var ok bool
		nameSymbol, ok = nameNode.(Symbol)
		if !ok {
			return nil, ErrExpectedSymbol
		}
	} else {
		nameSymbol = nulSymbol
	}

	var errEarlyReturns *ErrEarlyReturns
	rv, err := Progn(ctx, w, statements)
	if errors.As(err, &errEarlyReturns) && errEarlyReturns.Name == nameSymbol {
		return errEarlyReturns.Value, nil
	}
	return rv, err
}

type ErrThrown struct {
	Value   Node
	TagForm Node
}

func (e *ErrThrown) Error() string {
	return fmt.Sprintf("Thrown tag-form %s was not caught", ToString(e.TagForm, PRINT))
}
func cmdCatch(ctx context.Context, w *World, node Node) (Node, error) {
	// from ISLisp
	tagForm, statements, err := w.ShiftAndEvalCar(ctx, node)
	if err != nil {
		return nil, err
	}

	var errThrown *ErrThrown
	rv, err := Progn(ctx, w, statements)
	if errors.As(err, &errThrown) && errThrown.TagForm.Equals(tagForm, EQUALP) {
		return errThrown.Value, nil
	}
	return rv, err
}

func funThrow(ctx context.Context, w *World, list []Node) (Node, error) {
	// from ISLisp
	return nil, &ErrThrown{Value: list[1], TagForm: list[0]}
}

func cmdCond(ctx context.Context, w *World, list Node) (Node, error) {
	for HasValue(list) {
		var condAndAct Node
		var err error

		condAndAct, list, err = Shift(list)
		if err != nil {
			return nil, err
		}
		cond, act, err := w.ShiftAndEvalCar(ctx, condAndAct)
		if err != nil {
			return nil, err
		}
		if HasValue(cond) {
			return Progn(ctx, w, act)
		}
	}
	return Null, nil
}

var symbolT = NewSymbol("t")

func cmdCase(ctx context.Context, w *World, list Node) (Node, error) {
	var swValue Node
	var err error

	swValue, list, err = w.ShiftAndEvalCar(ctx, list)
	if err != nil {
		return nil, err
	}
	for HasValue(list) {
		var caseAndAct Node
		var err error

		caseAndAct, list, err = Shift(list)
		if err != nil {
			return nil, err
		}
		caseValue, act, err := Shift(caseAndAct)
		if err != nil {
			return nil, err
		}
		if cons, ok := caseValue.(*Cons); ok {
			var list Node = cons
			for HasValue(list) {
				var _caseValue Node
				_caseValue, list, err = w.ShiftAndEvalCar(ctx, list)
				if err != nil {
					return nil, err
				}
				if swValue.Equals(_caseValue, EQUALP) {
					return Progn(ctx, w, act)
				}
			}
		} else if caseValue.Equals(symbolT, STRICT) {
			return Progn(ctx, w, act)
		}
	}
	return Null, nil
}

func cmdIf(ctx context.Context, w *World, params Node) (Node, error) {
	cond, params, err := w.ShiftAndEvalCar(ctx, params)
	if err != nil {
		return nil, err
	}
	thenClause, params, err := Shift(params)
	if err != nil {
		return nil, err
	}
	var elseClause Node = Null
	if HasValue(params) {
		elseClause, params, err = Shift(params)
		if err != nil {
			return nil, err
		}
		if HasValue(params) {
			return nil, ErrTooManyArguments
		}
	}
	if HasValue(cond) {
		return thenClause.Eval(ctx, w)
	} else if HasValue(elseClause) {
		return elseClause.Eval(ctx, w)
	} else {
		return Null, nil
	}
}

func cmdWhile(ctx context.Context, w *World, n Node) (Node, error) {
	// from autolisp, ISLisp
	cond, statements, err := Shift(n)
	if err != nil {
		return nil, err
	}
	var last Node = Null
	for {
		if err := CheckContext(ctx); err != nil {
			return nil, err
		}
		cont, err := cond.Eval(ctx, w)
		if err != nil {
			return nil, err
		}
		if IsNull(cont) {
			return last, nil
		}
		last, err = Progn(ctx, w, statements)
		if err != nil {
			return nil, err
		}
	}
}

func cmdQuit(context.Context, *World, Node) (Node, error) {
	return Null, ErrQuit
}

func cmdFor(ctx context.Context, w *World, list Node) (Node, error) {
	// CommonLisp's DO and ISLisp's FOR
	var vars Node
	var err error

	vars, list, err = Shift(list)
	if err != nil {
		return nil, err
	}
	stepsCalc := make([]func() (Node, error), 0, 8)
	stepsAssign := make([]func(Node) error, 0, 8)
	for HasValue(vars) {
		var varInitStep Node
		var var1 Node
		var initv Node
		var step Node

		varInitStep, vars, err = Shift(vars)
		if err != nil {
			return nil, err
		}
		var1, varInitStep, err = Shift(varInitStep)
		if err != nil {
			return nil, err
		}
		symbol, ok := var1.(Symbol)
		if !ok {
			return nil, ErrExpectedSymbol
		}
		initv, varInitStep, err = w.ShiftAndEvalCar(ctx, varInitStep)
		if err != nil {
			return nil, err
		}
		if err = w.Set(symbol, initv); err != nil {
			return nil, err
		}
		step, varInitStep, err = Shift(varInitStep)
		if err != nil {
			return nil, err
		}
		stepsCalc = append(stepsCalc, func() (Node, error) {
			return step.Eval(ctx, w)
		})
		stepsAssign = append(stepsAssign, func(value Node) error {
			return w.Set(symbol, value)
		})
	}
	var conds Node
	var cond Node
	var result Node

	conds, list, err = Shift(list)
	if err != nil {
		return nil, err
	}
	cond, conds, err = Shift(conds)
	if err != nil {
		return nil, err
	}
	result, conds, err = Shift(conds)
	if err != nil {
		return nil, err
	}
	valuesStep := make([]Node, len(stepsCalc))
	for {
		value, err := cond.Eval(ctx, w)
		if err != nil {
			return nil, err
		}
		if HasValue(value) {
			return result.Eval(ctx, w)
		}
		_, err = Progn(ctx, w, list)
		if err != nil {
			return nil, err
		}
		for i, step1 := range stepsCalc {
			valuesStep[i], err = step1()
			if err != nil {
				return nil, err
			}
		}
		for i, step1 := range stepsAssign {
			if err = step1(valuesStep[i]); err != nil {
				return nil, err
			}
		}
	}
}

func handlerCaseSub(ctx context.Context, w *World, caseBlock Node, c Node) (Node, error) {
	paramList, caseBlock, err := Shift(caseBlock)
	if err != nil {
		return nil, err
	}
	if IsNull(paramList) { // (error () ... )
		return Progn(ctx, w, caseBlock)
	}
	// (error (c) ... )
	conditionVarName, paramList, err := Shift(paramList)
	if err != nil {
		return nil, err
	}
	if HasValue(paramList) {
		return nil, ErrTooManyArguments
	}
	symbol, ok := conditionVarName.(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}
	newWorld := w.Let(&Pair{Key: symbol, Value: c})
	return Progn(ctx, newWorld, caseBlock)
}

type ErrorNode struct {
	Value error
}

func (e *ErrorNode) PrintTo(w io.Writer, m PrintMode) (int, error) {
	return io.WriteString(w, e.Value.Error())
}

func (e *ErrorNode) Eval(context.Context, *World) (Node, error) {
	return e, nil
}

func (e *ErrorNode) Equals(n Node, m EqlMode) bool {
	f, ok := n.(*ErrorNode)
	if !ok {
		return false
	}
	return errors.Is(e.Value, f.Value) || errors.Is(f.Value, e.Value)
}

func matchError(ctx context.Context, w *World, casedSymbol Node, happenError error) (bool, error) {
	if happenError == nil {
		return false, nil
	}
	if casedSymbol == NewSymbol("error") {
		return true, nil
	}
	casedNode, err := casedSymbol.Eval(ctx, w)
	if err != nil {
		return false, err
	}
	errNode, ok := casedNode.(*ErrorNode)
	if !ok {
		return false, fmt.Errorf("not an error object in hander-case: %s", ToString(casedSymbol, PRINT))
	}
	return errors.Is(happenError, errNode.Value), nil
}

func cmdWithHandler(ctx context.Context, w *World, list Node) (Node, error) {
	// ISLisp
	handlerNode, list, err := w.ShiftAndEvalCar(ctx, list)
	if err != nil {
		return nil, err
	}
	handler, ok := handlerNode.(Callable)
	if !ok {
		return nil, ErrExpectedFunction
	}
	value, err := Progn(ctx, w, list)
	if err == nil {
		return value, nil
	}
	_, err2 := handler.Call(ctx, w, &Cons{Car: &ErrorNode{Value: err}, Cdr: Null})
	if err2 != nil {
		return nil, err2
	}
	return nil, err
}

func cmdUnwindProtect(ctx context.Context, w *World, list Node) (Node, error) {
	var formErr error

	_, list, formErr = w.ShiftAndEvalCar(ctx, list)

	value, err := Progn(ctx, w, list)
	if err != nil {
		return nil, err
	}
	if formErr != nil {
		return nil, formErr
	}
	return value, nil
}
