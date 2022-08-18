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

func funReturn(ctx context.Context, w *World, argv []Node) (Node, error) {
	// from CommonLisp
	return nil, &ErrEarlyReturns{Value: argv[0], Name: ""}
}

func cmdReturnFrom(ctx context.Context, w *World, n Node) (Node, error) {
	// from CommonLisp
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
	value = Null
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
	// from CommonLisp
	nameNode, statements, err := shift(node)
	if err != nil {
		return nil, err
	}
	nameSymbol, ok := nameNode.(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}
	name := string(nameSymbol)

	var errEarlyReturns *ErrEarlyReturns
	rv, err := progn(ctx, w, statements)
	if errors.As(err, &errEarlyReturns) && errEarlyReturns.Name == name {
		return errEarlyReturns.Value, nil
	}
	return rv, err
}

type ErrThrown struct {
	Value   Node
	TagForm Node
}

func (e *ErrThrown) Error() string {
	return fmt.Sprintf("Thrown tag-form %s was not caught", toString(e.TagForm, PRINT))
}
func cmdCatch(ctx context.Context, w *World, node Node) (Node, error) {
	// from ISLisp
	tagForm, statements, err := w.shiftAndEvalCar(ctx, node)
	if err != nil {
		return nil, err
	}

	var errThrown *ErrThrown
	rv, err := progn(ctx, w, statements)
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

		condAndAct, list, err = shift(list)
		if err != nil {
			return nil, err
		}
		cond, act, err := w.shiftAndEvalCar(ctx, condAndAct)
		if err != nil {
			return nil, err
		}
		if HasValue(cond) {
			return progn(ctx, w, act)
		}
	}
	return Null, nil
}

func cmdCase(ctx context.Context, w *World, list Node) (Node, error) {
	var swValue Node
	var err error

	swValue, list, err = w.shiftAndEvalCar(ctx, list)
	if err != nil {
		return nil, err
	}
	for HasValue(list) {
		var caseAndAct Node
		var err error

		caseAndAct, list, err = shift(list)
		if err != nil {
			return nil, err
		}
		caseValue, act, err := shift(caseAndAct)
		if err != nil {
			return nil, err
		}
		if cons, ok := caseValue.(*Cons); ok {
			var list Node = cons
			for HasValue(list) {
				var _caseValue Node
				_caseValue, list, err = w.shiftAndEvalCar(ctx, list)
				if err != nil {
					return nil, err
				}
				if swValue.Equals(_caseValue, EQUALP) {
					return progn(ctx, w, act)
				}
			}
		} else {
			_caseValue, err := caseValue.Eval(ctx, w)
			if err != nil {
				return nil, err
			}
			if swValue.Equals(_caseValue, EQUALP) {
				return progn(ctx, w, act)
			}
		}
	}
	return Null, nil
}

func cmdIf(ctx context.Context, w *World, params Node) (Node, error) {
	cond, params, err := w.shiftAndEvalCar(ctx, params)
	if err != nil {
		return nil, err
	}
	thenClause, params, err := shift(params)
	if err != nil {
		return nil, err
	}
	var elseClause Node = Null
	if HasValue(params) {
		elseClause, params, err = shift(params)
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

func cmdWhen(ctx context.Context, w *World, args Node) (Node, error) {
	cond, args, err := w.shiftAndEvalCar(ctx, args)
	if err != nil {
		return nil, err
	}
	if IsNull(cond) {
		return Null, nil
	}
	return progn(ctx, w, args)
}

func cmdUnless(ctx context.Context, w *World, args Node) (Node, error) {
	cond, args, err := w.shiftAndEvalCar(ctx, args)
	if err != nil {
		return nil, err
	}
	if HasValue(cond) {
		return Null, nil
	}
	return progn(ctx, w, args)
}

func cmdForeach(ctx context.Context, w *World, args Node) (Node, error) {
	// from autolisp
	var _symbol Node
	var err error

	_symbol, args, err = shift(args)
	if err != nil {
		return nil, err
	}
	symbol, ok := _symbol.(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}

	var list Node
	var code Node
	list, code, err = w.shiftAndEvalCar(ctx, args)
	if err != nil {
		return nil, err
	}

	var last Node
	for HasValue(list) {
		var value Node

		value, list, err = shift(list)
		if err != nil {
			return nil, err
		}
		if err := w.Set(symbol, value); err != nil {
			return nil, err
		}

		last, err = progn(ctx, w, code)
		if err != nil {
			return nil, err
		}
	}
	return last, nil
}

func cmdWhile(ctx context.Context, w *World, n Node) (Node, error) {
	// from autolisp, ISLisp
	cond, statements, err := shift(n)
	if err != nil {
		return nil, err
	}
	var last Node = Null
	for {
		if err := checkContext(ctx); err != nil {
			return nil, err
		}
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

func cmdDoTimes(ctx context.Context, w *World, list Node) (Node, error) {
	// from CommonLisp
	var varAndValueNode Node
	var err error

	varAndValueNode, list, err = shift(list)
	if err != nil {
		return nil, err
	}
	var varAndValueArray [2]Node
	if err := listToArray(varAndValueNode, varAndValueArray[:]); err != nil {
		return nil, err
	}
	symbol, ok := varAndValueArray[0].(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}
	endNode, err := varAndValueArray[1].Eval(ctx, w)
	if err != nil {
		return nil, err
	}
	end, ok := endNode.(Integer)
	if !ok {
		return nil, ErrExpectedNumber
	}

	var last Node = Null
	for i := Integer(0); i < end; i++ {
		if err := checkContext(ctx); err != nil {
			return nil, err
		}
		if err := w.Set(symbol, i); err != nil {
			return nil, err
		}
		last, err = progn(ctx, w, list)
		if err != nil {
			return nil, err
		}
	}
	return last, nil
}

func cmdDoList(ctx context.Context, w *World, list Node) (Node, error) {
	// from CommonLisp
	var varAndValueNode Node
	var err error

	varAndValueNode, list, err = shift(list)
	if err != nil {
		return nil, err
	}
	var varAndValues [2]Node
	if err := listToArray(varAndValueNode, varAndValues[:]); err != nil {
		return nil, err
	}
	symbol, ok := varAndValues[0].(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}
	var last Node = Null
	values, err := varAndValues[1].Eval(ctx, w)
	if err != nil {
		return nil, err
	}
	for HasValue(values) {
		if err := checkContext(ctx); err != nil {
			return nil, err
		}
		var value1 Node

		value1, values, err = shift(values)
		if err != nil {
			return nil, err
		}
		if err := w.Set(symbol, value1); err != nil {
			return nil, err
		}
		last, err = progn(ctx, w, list)
		if err != nil {
			return nil, err
		}
	}
	return last, nil
}

func cmdDoAndFor(ctx context.Context, w *World, list Node) (Node, error) {
	// CommonLisp's DO and ISLisp's FOR
	var vars Node
	var err error

	vars, list, err = shift(list)
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

		varInitStep, vars, err = shift(vars)
		if err != nil {
			return nil, err
		}
		var1, varInitStep, err = shift(varInitStep)
		if err != nil {
			return nil, err
		}
		symbol, ok := var1.(Symbol)
		if !ok {
			return nil, ErrExpectedSymbol
		}
		initv, varInitStep, err = w.shiftAndEvalCar(ctx, varInitStep)
		if err != nil {
			return nil, err
		}
		if err = w.Set(symbol, initv); err != nil {
			return nil, err
		}
		step, varInitStep, err = shift(varInitStep)
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

	conds, list, err = shift(list)
	if err != nil {
		return nil, err
	}
	cond, conds, err = shift(conds)
	if err != nil {
		return nil, err
	}
	result, conds, err = shift(conds)
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
		_, err = progn(ctx, w, list)
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
	paramList, caseBlock, err := shift(caseBlock)
	if err != nil {
		return nil, err
	}
	if IsNull(paramList) { // (error () ... )
		return progn(ctx, w, caseBlock)
	}
	// (error (c) ... )
	conditionVarName, paramList, err := shift(paramList)
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
	newWorld := &World{
		globals: map[Symbol]Node{symbol: c},
		parent:  w,
	}
	return progn(ctx, newWorld, caseBlock)
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
	if casedSymbol == Symbol("error") {
		return true, nil
	}
	casedNode, err := casedSymbol.Eval(ctx, w)
	if err != nil {
		return false, err
	}
	errNode, ok := casedNode.(*ErrorNode)
	if !ok {
		return false, fmt.Errorf("not an error object in hander-case: %s", toString(casedSymbol, PRINT))
	}
	return errors.Is(happenError, errNode.Value), nil
}

func cmdHandlerCase(ctx context.Context, w *World, list Node) (Node, error) {
	tryCommand, list, err := shift(list)
	if err != nil {
		return nil, err
	}
	value, happenError := tryCommand.Eval(ctx, w)
	for HasValue(list) {
		var caseBlock Node
		var err error

		caseBlock, list, err = shift(list)
		if err != nil {
			return nil, err
		}
		casedError, caseBlock, err := shift(caseBlock)
		if err != nil {
			return nil, err
		}
		ok, err := matchError(ctx, w, casedError, happenError)
		if err != nil {
			return nil, err
		}
		if ok {
			value, err := handlerCaseSub(ctx, w, caseBlock, &ErrorNode{Value: happenError})
			if err != nil {
				return nil, fmt.Errorf("error: %w", err)
			}
			return value, nil
		} else if happenError == nil && casedError == Keyword(":no-error") {
			value, err := handlerCaseSub(ctx, w, caseBlock, value)
			if err != nil {
				return nil, fmt.Errorf(":no-error: %w", err)
			}
			return value, nil
		}
	}
	return Null, nil
}

func cmdWithHandler(ctx context.Context, w *World, list Node) (Node, error) {
	// ISLisp
	handlerNode, list, err := w.shiftAndEvalCar(ctx, list)
	if err != nil {
		return nil, err
	}
	handler, ok := handlerNode.(_Callable)
	if !ok {
		return nil, ErrExpectedFunction
	}
	value, err := progn(ctx, w, list)
	if err == nil {
		return value, nil
	}
	_, err2 := handler.Call(ctx, w, &Cons{Car: &ErrorNode{Value: err}, Cdr: Null})
	if err2 != nil {
		return nil, err2
	}
	return nil, err
}
