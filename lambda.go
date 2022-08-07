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
	rest    Symbol
	lexical *World
}

func cmdLambda(_ context.Context, w *World, node Node) (Node, error) {
	return newLambda(w, node, "")
}

type _Parameters struct {
	param []Symbol
	rest  Symbol
	code  Node
}

func getParameterList(node Node) (*_Parameters, error) {
	list, code, err := shift(node)
	if err != nil {
		return nil, err
	}
	params := []Symbol{}
	var rest Symbol
	for HasValue(list) {
		var nameNode Node

		nameNode, list, err = shift(list)
		if err != nil {
			return nil, err
		}
		nameSymbol, ok := nameNode.(Symbol)
		if !ok {
			return nil, ErrExpectedSymbol
		}
		if nameSymbol == "&rest" {
			nameNode, list, err = shift(list)
			if err != nil {
				return nil, err
			}
			rest, ok = nameNode.(Symbol)
			if !ok {
				return nil, ErrExpectedSymbol
			}
			continue
		}
		params = append(params, nameSymbol)
	}
	return &_Parameters{
		param: params,
		rest:  rest,
		code:  code,
	}, nil
}

func newLambda(w *World, node Node, blockName Symbol) (Node, error) {
	p, err := getParameterList(node)
	if err != nil {
		return nil, err
	}
	return &_Lambda{
		param:   p.param,
		code:    p.code,
		name:    blockName,
		rest:    p.rest,
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

	if HasValue(n) && L.rest == "" {
		return nil, ErrTooManyArguments
	}
	if L.rest != "" {
		globals[L.rest] = n
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

func cmdApply(ctx context.Context, w *World, list Node) (Node, error) {
	funcNode, list, err := w.shiftAndEvalCar(ctx, list)
	if err != nil {
		return nil, err
	}
	f, ok := funcNode.(_Callable)
	if !ok {
		return nil, ErrExpectedFunction
	}
	var newargs _ListBuilder
	for {
		var value Node

		value, list, err = shift(list)
		if err != nil {
			return nil, err
		}
		if IsNull(list) {
			// value is the last argument = array
			value, err = value.Eval(ctx, w)
			if err != nil {
				return nil, err
			}
			seqEach(value, func(n Node) error {
				newargs.Add(n)
				return nil
			})
			return f.Call(ctx, w, newargs.Sequence())
		}
		newargs.Add(value)
	}
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

type Function struct {
	C int
	F func(context.Context, *World, []Node) (Node, error)
}

func (*Function) PrintTo(w io.Writer, m PrintMode) {
	io.WriteString(w, "buildin function")
}

func (f *Function) Eval(context.Context, *World) (Node, error) {
	return f, nil
}

func (f *Function) Equals(n Node, m EqlMode) bool {
	return false
}

const maxParameterOfEasyFunc = 8

func (f *Function) Call(ctx context.Context, w *World, list Node) (Node, error) {
	if err := checkContext(ctx); err != nil {
		return nil, err
	}
	if f.C >= 0 {
		var argv [maxParameterOfEasyFunc]Node
		for i := 0; i < f.C; i++ {
			var err error

			argv[i], list, err = w.shiftAndEvalCar(ctx, list)
			if err != nil {
				return nil, err
			}
		}
		if HasValue(list) {
			return nil, ErrTooManyArguments
		}
		return f.F(ctx, w, argv[:f.C])
	} else {
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
		return f.F(ctx, w, argv)
	}
}

type KWFunction struct {
	C int
	F func(context.Context, *World, []Node, map[Keyword]Node) (Node, error)
}

func (*KWFunction) PrintTo(w io.Writer, m PrintMode) {
	io.WriteString(w, "buildin function")
}

func (f *KWFunction) Eval(context.Context, *World) (Node, error) {
	return f, nil
}

func (f *KWFunction) Equals(n Node, m EqlMode) bool {
	return false
}

func (f *KWFunction) Call(ctx context.Context, w *World, list Node) (Node, error) {
	if err := checkContext(ctx); err != nil {
		return nil, err
	}
	args := []Node{}
	kwargs := map[Keyword]Node{}
	for HasValue(list) {
		var tmp Node
		var err error

		tmp, list, err = w.shiftAndEvalCar(ctx, list)
		if err != nil {
			return nil, err
		}
		if keyword, ok := tmp.(Keyword); ok {
			tmp, list, err = w.shiftAndEvalCar(ctx, list)
			if err != nil {
				return nil, err
			}
			kwargs[keyword] = tmp
		} else {
			args = append(args, tmp)
		}
	}
	return f.F(ctx, w, args, kwargs)
}
