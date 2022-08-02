package gmnlisp

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"
)

type _Lambda struct {
	param   []Symbol
	code    Node
	name    Symbol
	lexical *World
}

func cmdLambda(_ context.Context, w *World, node Node) (Node, error) {
	return newLambda(w, node, "")
}

func getParameterList(node Node) ([]Symbol, Node, error) {
	list, rest, err := shift(node)
	if err != nil {
		return nil, nil, err
	}
	params := []Symbol{}
	for HasValue(list) {
		var nameNode Node

		nameNode, list, err = shift(list)
		if err != nil {
			return nil, nil, err
		}
		nameSymbol, ok := nameNode.(Symbol)
		if !ok {
			return nil, nil, ErrExpectedSymbol
		}
		params = append(params, nameSymbol)
	}
	return params, rest, nil
}

func newLambda(w *World, node Node, blockName Symbol) (Node, error) {
	// (lambda (param) code)
	params, code, err := getParameterList(node)
	if err != nil {
		return nil, err
	}
	return &_Lambda{
		param:   params,
		code:    code,
		name:    blockName,
		lexical: w,
	}, nil
}

func (L *_Lambda) PrintTo(w io.Writer, m PrintMode) {
	io.WriteString(w, "(lambda (")
	dem := ""
	for _, name := range L.param {
		io.WriteString(w, dem)
		name.PrintTo(w, PRINC)
		dem = " "
	}
	io.WriteString(w, ") ")
	if cons, ok := L.code.(*Cons); ok {
		cons.writeToWithoutKakko(w, m)
	} else {
		L.code.PrintTo(w, m)
	}
	io.WriteString(w, ")")
}

var trace = map[Symbol]int{}

func (L *_Lambda) Call(ctx context.Context, w *World, n Node) (Node, error) {
	if err := checkContext(ctx); err != nil {
		return nil, err
	}
	globals := map[Symbol]Node{}
	foundSlash := false
	traceCount, traceDo := trace[L.name]
	if traceDo {
		fmt.Fprintf(os.Stderr, "[%d: (%s", traceCount, L.name)
		trace[L.name]++
		defer func() {
			trace[L.name]--
		}()
	}
	for _, name := range L.param {
		if name == "/" {
			foundSlash = true
			continue
		}
		if foundSlash {
			globals[name] = Null
			continue
		}
		var err error
		var value Node
		value, n, err = w.shiftAndEvalCar(ctx, n)
		if err != nil {
			return nil, err
		}
		globals[name] = value
		if traceDo {
			fmt.Fprintf(os.Stderr, " %s", toString(value, PRINT))
		}
	}
	if traceDo {
		fmt.Fprintln(os.Stderr, ")]")
	}

	if HasValue(n) {
		return nil, ErrTooManyArguments
	}
	newWorld := &World{globals: globals, parent: L.lexical}

	var errEarlyReturns *ErrEarlyReturns

	result, err := progn(ctx, newWorld, L.code)
	if errors.As(err, &errEarlyReturns) && errEarlyReturns.Name == string(L.name) {
		return errEarlyReturns.Value, nil
	}
	if traceDo {
		fmt.Fprintf(os.Stderr, "[%d: %s returned %s]\n",
			traceCount,
			L.name,
			toString(result, PRINT))
	}
	return result, err
}

func (L *_Lambda) Eval(context.Context, *World) (Node, error) {
	return L, nil
}

func (*_Lambda) Equals(Node, EqlMode) bool {
	return false
}

func cmdDefun(_ context.Context, w *World, list Node) (Node, error) {
	_symbol, list, err := shift(list)
	if err != nil {
		return nil, err
	}
	symbol, ok := _symbol.(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}
	lambda, err := newLambda(w, list, symbol)
	if err != nil {
		return nil, err
	}
	w.SetOrDefineParameter(symbol, lambda)
	return symbol, nil
}

func cmdFunCall(ctx context.Context, w *World, node Node) (Node, error) {
	f, node, err := w.shiftAndEvalCar(ctx, node)
	if err != nil {
		return nil, err
	}
	_f, ok := f.(_Callable)
	if !ok {
		return nil, ErrExpectedFunction
	}
	return _f.Call(ctx, w, node)
}

func funApply(ctx context.Context, w *World, list []Node) (Node, error) {
	f, ok := list[0].(_Callable)
	if !ok {
		return nil, ErrExpectedFunction
	}
	cons := list[len(list)-1]
	for i := len(list) - 2; i > 0; i-- {
		cons = &Cons{
			Car: list[i],
			Cdr: cons,
		}
	}
	return f.Call(ctx, w, cons)
}

type _Callable interface {
	Node
	Call(context.Context, *World, Node) (Node, error)
}

type SpecialF func(context.Context, *World, Node) (Node, error)

func (SpecialF) PrintTo(w io.Writer, m PrintMode) {
	io.WriteString(w, "buildin function")
}

func (f SpecialF) Eval(context.Context, *World) (Node, error) {
	return f, nil
}

func (f SpecialF) Equals(n Node, m EqlMode) bool {
	return false
}

func (f SpecialF) Call(ctx context.Context, w *World, n Node) (Node, error) {
	if err := checkContext(ctx); err != nil {
		return nil, err
	}
	return f(ctx, w, n)
}

func funFunction(_ context.Context, _ *World, argv []Node) (Node, error) {
	f, ok := argv[0].(_Callable)
	if !ok {
		return nil, ErrExpectedFunction
	}
	return f, nil
}

func cmdTrace(_ context.Context, _ *World, list Node) (Node, error) {
	// from CommonLisp
	if len(trace) > 0 {
		trace = map[Symbol]int{}
	}
	for HasValue(list) {
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
		trace[symbol] = 0
	}
	return Null, nil
}

type FixArgsF struct {
	C int
	F func(context.Context, *World, []Node) (Node, error)
}

func (*FixArgsF) PrintTo(w io.Writer, m PrintMode) {
	io.WriteString(w, "buildin function")
}

func (f *FixArgsF) Eval(context.Context, *World) (Node, error) {
	return f, nil
}

func (f *FixArgsF) Equals(n Node, m EqlMode) bool {
	return false
}

const maxParameterOfEasyFunc = 8

func (f *FixArgsF) Call(ctx context.Context, w *World, list Node) (Node, error) {
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
	return f.F(ctx, w, argv[:f.C])
}

type VarArgsF func(context.Context, *World, []Node) (Node, error)

func (f VarArgsF) PrintTo(w io.Writer, m PrintMode) {
	io.WriteString(w, "built-in function(N)")
}

func (f VarArgsF) Eval(context.Context, *World) (Node, error) {
	return f, nil
}

func (f VarArgsF) Equals(n Node, m EqlMode) bool {
	return false
}

func (f VarArgsF) Call(ctx context.Context, w *World, list Node) (Node, error) {
	if err := checkContext(ctx); err != nil {
		return nil, err
	}
	argv := []Node{}
	for HasValue(list) {
		var tmp Node
		var err error

		tmp, list, err = w.shiftAndEvalCar(ctx, list)
		if err != nil {
			return nil, err
		}
		argv = append(argv, tmp)
	}
	return f(ctx, w, argv)
}
