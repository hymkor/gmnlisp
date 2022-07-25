package gmnlisp

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"
)

type _Lambda struct {
	param   []string
	code    Node
	name    string
	lexical *World
}

func cmdLambda(ctx context.Context, w *World, node Node) (Node, error) {
	return newLambda(w, node, "")
}

func getParameterList(node Node) ([]string, Node, error) {
	list, rest, err := shift(node)
	if err != nil {
		return nil, nil, err
	}
	params := []string{}
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
		params = append(params, string(nameSymbol))
	}
	return params, rest, nil
}

func newLambda(w *World, node Node, blockName string) (Node, error) {
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
		io.WriteString(w, name)
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

var trace = map[string]int{}

func (L *_Lambda) Call(ctx context.Context, w *World, n Node) (Node, error) {
	if err := checkContext(ctx); err != nil {
		return nil, err
	}
	globals := map[string]Node{}
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
			fmt.Fprintf(os.Stderr, " %s", toString(value))
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
			toString(result))
	}
	return result, err
}

func (L *_Lambda) Eval(context.Context, *World) (Node, error) {
	return L, nil
}

func (*_Lambda) Equals(Node, EqlMode) bool {
	return false
}

func cmdDefun(ctx context.Context, w *World, list Node) (Node, error) {
	_symbol, list, err := shift(list)
	if err != nil {
		return nil, err
	}
	symbol, ok := _symbol.(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}
	name := string(symbol)

	lambda, err := newLambda(w, list, name)
	if err != nil {
		return nil, err
	}
	w.Set(name, lambda)
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

type _Callable interface {
	Node
	Call(context.Context, *World, Node) (Node, error)
}

type Function func(context.Context, *World, Node) (Node, error)

func (Function) PrintTo(w io.Writer, m PrintMode) {
	io.WriteString(w, "buildin function")
}

func (f Function) Eval(context.Context, *World) (Node, error) {
	return f, nil
}

func (f Function) Equals(n Node, m EqlMode) bool {
	return false
}

func (f Function) Call(ctx context.Context, w *World, n Node) (Node, error) {
	if err := checkContext(ctx); err != nil {
		return nil, err
	}
	return f(ctx, w, n)
}

func cmdFunction(ctx context.Context, w *World, node Node) (Node, error) {
	var argv [1]Node
	if err := w.evalListAll(ctx, node, argv[:]); err != nil {
		return nil, err
	}
	f, ok := argv[0].(_Callable)
	if !ok {
		return nil, ErrExpectedFunction
	}
	return f, nil
}

func cmdTrace(ctx context.Context, w *World, list Node) (Node, error) {
	if len(trace) > 0 {
		trace = map[string]int{}
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
		trace[string(symbol)] = 0
	}
	return Null, nil
}
