package gmnlisp

import (
	"context"
	"errors"
	"fmt"
	"io"
	"math"
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
	return newLambda(w, node, nulSymbol)
}

type _Parameters struct {
	param []Symbol
	rest  Symbol
	code  Node
}

func getParameterList(node Node) (*_Parameters, error) {
	list, code, err := Shift(node)
	if err != nil {
		return nil, err
	}
	params := []Symbol{}
	rest := nulSymbol
	for HasValue(list) {
		var nameNode Node

		nameNode, list, err = Shift(list)
		if err != nil {
			return nil, err
		}
		if nameNode == ampRest || nameNode == colonRest {
			nameNode, list, err = Shift(list)
			if err != nil {
				return nil, err
			}
			var ok bool
			rest, ok = nameNode.(Symbol)
			if !ok {
				return nil, ErrExpectedSymbol
			}
		} else {
			nameSymbol, ok := nameNode.(Symbol)
			if !ok {
				return nil, fmt.Errorf("%w: %#v", ErrExpectedSymbol, nameNode)
			}
			params = append(params, nameSymbol)
		}

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

func (L *_Lambda) PrintTo(w io.Writer, m PrintMode) (int, error) {
	io.WriteString(w, "(lambda (")
	dem := ""
	siz := 0
	for _, name := range L.param {
		io.WriteString(w, dem)
		_siz, _ := name.PrintTo(w, PRINC)
		siz += _siz
		dem = " "
	}
	io.WriteString(w, ") ")
	if cons, ok := L.code.(*Cons); ok {
		_siz, _ := cons.writeToWithoutKakko(w, m)
		siz += _siz
	} else {
		_siz, _ := L.code.PrintTo(w, m)
		siz += _siz
	}
	_siz, _ := io.WriteString(w, ")")
	siz += _siz
	return siz, nil
}

var trace = map[Symbol]int{}

func (L *_Lambda) Call(ctx context.Context, w *World, n Node) (Node, error) {
	if err := CheckContext(ctx); err != nil {
		return nil, fmt.Errorf("%#v: %w", L.name, err)
	}
	lexical := Variables{}
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
		if name == slashSymbol {
			foundSlash = true
			continue
		}
		if foundSlash {
			lexical[name] = Null
			continue
		}
		var err error
		var value Node
		value, n, err = w.ShiftAndEvalCar(ctx, n)
		if err != nil {
			return nil, fmt.Errorf("%s: %w", L.name, err)
		}
		lexical[name] = value
		if traceDo {
			fmt.Fprintf(os.Stderr, " %#v", value)
		}
	}
	if traceDo {
		fmt.Fprintln(os.Stderr, ")]")
	}

	if HasValue(n) && L.rest == nulSymbol {
		return nil, fmt.Errorf("%s: %w", L.name, ErrTooManyArguments)
	}
	if L.rest != nulSymbol {
		var values Node = nil
		for HasValue(n) {
			var value Node
			var err error

			value, n, err = w.ShiftAndEvalCar(ctx, n)
			if err != nil {
				return nil, err
			}
			values = &Cons{
				Car: value,
				Cdr: values,
			}
		}
		var err error
		lexical[L.rest], err = NReverse(values)
		if err != nil {
			return nil, err
		}
	}
	newWorld := L.lexical.Let(lexical)

	var errEarlyReturns *ErrEarlyReturns

	result, err := Progn(ctx, newWorld, L.code)
	if errors.As(err, &errEarlyReturns) && errEarlyReturns.Name == L.name {
		return errEarlyReturns.Value, nil
	}
	if traceDo {
		fmt.Fprintf(os.Stderr, "[%d: %s returned %#v]\n",
			traceCount,
			L.name,
			result)
	}
	if err != nil {
		return nil, fmt.Errorf("%s: %w", L.name, err)
	}
	return result, nil
}

func (L *_Lambda) Eval(context.Context, *World) (Node, error) {
	return L, nil
}

func (*_Lambda) Equals(Node, EqlMode) bool {
	return false
}

func cmdDefun(_ context.Context, w *World, list Node) (Node, error) {
	_symbol, list, err := Shift(list)
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
	f, node, err := w.ShiftAndEvalCar(ctx, node)
	if err != nil {
		return nil, err
	}
	_f, ok := f.(Callable)
	if !ok {
		return nil, ErrExpectedFunction
	}
	return _f.Call(ctx, w, node)
}

func cmdApply(ctx context.Context, w *World, list Node) (Node, error) {
	funcNode, list, err := w.ShiftAndEvalCar(ctx, list)
	if err != nil {
		return nil, err
	}
	f, ok := funcNode.(Callable)
	if !ok {
		return nil, ErrExpectedFunction
	}
	var newargs ListBuilder
	for {
		var value Node

		value, list, err = Shift(list)
		if err != nil {
			return nil, err
		}
		if IsNull(list) {
			// value is the last argument = array
			value, err = value.Eval(ctx, w)
			if err != nil {
				return nil, err
			}
			SeqEach(value, func(n Node) error {
				newargs.Add(n)
				return nil
			})
			return f.Call(ctx, w, newargs.Sequence())
		}
		newargs.Add(value)
	}
}

type Callable interface {
	Node
	Call(context.Context, *World, Node) (Node, error)
}

type SpecialF func(context.Context, *World, Node) (Node, error)

func (SpecialF) PrintTo(w io.Writer, m PrintMode) (int, error) {
	return io.WriteString(w, "buildin function")
}

func (f SpecialF) Eval(context.Context, *World) (Node, error) {
	return f, nil
}

func (f SpecialF) Equals(n Node, m EqlMode) bool {
	return false
}

func (f SpecialF) Call(ctx context.Context, w *World, n Node) (Node, error) {
	if err := CheckContext(ctx); err != nil {
		return nil, err
	}
	return f(ctx, w, n)
}

func funFunction(_ context.Context, _ *World, argv []Node) (Node, error) {
	f, ok := argv[0].(Callable)
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

		symbolNode, list, err = Shift(list)
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
	C   int
	F   func(context.Context, *World, []Node) (Node, error)
	Min int
	Max int
}

func (*Function) PrintTo(w io.Writer, m PrintMode) (int, error) {
	return io.WriteString(w, "buildin function")
}

func (f *Function) Eval(context.Context, *World) (Node, error) {
	return f, nil
}

func (f *Function) Equals(n Node, m EqlMode) bool {
	return false
}

const maxParameterOfEasyFunc = 8

func (f *Function) Call(ctx context.Context, w *World, list Node) (Node, error) {
	if err := CheckContext(ctx); err != nil {
		return nil, err
	}
	max := math.MaxInt
	if f.Max > 0 {
		max = f.Max
	} else if f.C > 0 {
		max = f.C
	}

	min := 0
	if f.C > 0 {
		min = f.C
	} else if f.Min > 0 {
		min = f.Min
	}

	args := []Node{}
	for HasValue(list) {
		var tmp Node
		var err error

		tmp, list, err = w.ShiftAndEvalCar(ctx, list)
		if err != nil {
			return nil, err
		}
		args = append(args, tmp)
	}

	if len(args) > max {
		return nil, ErrTooManyArguments
	}
	if len(args) < min {
		return nil, ErrTooFewArguments
	}
	return f.F(ctx, w, args)
}

func cmdFlet(ctx context.Context, w *World, list Node) (Node, error) {
	flist, list, err := Shift(list)
	if err != nil {
		return nil, err
	}
	lexical := Variables{}
	for HasValue(flist) {
		var flist1 Node
		flist1, flist, err = Shift(flist)
		if err != nil {
			return nil, err
		}
		var name Node
		name, flist1, err = Shift(flist1)
		if err != nil {
			return nil, err
		}
		symbol, ok := name.(Symbol)
		if !ok {
			return nil, ErrExpectedSymbol
		}
		lambda, err := newLambda(w, flist1, symbol)
		if err != nil {
			return nil, err
		}
		lexical[symbol] = lambda
	}
	nw := w.Let(lexical)
	return Progn(ctx, nw, list)
}

func cmdLabels(ctx context.Context, w *World, list Node) (Node, error) {
	flist, list, err := Shift(list)
	if err != nil {
		return nil, err
	}
	lexical := Variables{}
	nw := w.Let(lexical)
	for HasValue(flist) {
		var flist1 Node
		flist1, flist, err = Shift(flist)
		if err != nil {
			return nil, err
		}
		var name Node
		name, flist1, err = Shift(flist1)
		if err != nil {
			return nil, err
		}
		symbol, ok := name.(Symbol)
		if !ok {
			return nil, ErrExpectedSymbol
		}
		lambda, err := newLambda(nw, flist1, symbol)
		if err != nil {
			return nil, err
		}
		lexical[symbol] = lambda
	}
	return Progn(ctx, nw, list)
}

type LispString struct {
	S       string
	compile Node
}

func (L *LispString) Eval(ctx context.Context, w *World) (Node, error) {
	if L.compile == nil {
		c, err := w.Interpret(ctx, L.S)
		if err != nil {
			return nil, err
		}
		L.compile = c
	}
	return L.compile, nil
}

func (L *LispString) PrintTo(w io.Writer, m PrintMode) (int, error) {
	return io.WriteString(w, L.S)
}

func (L *LispString) Equals(_other Node, m EqlMode) bool {
	other, ok := _other.(*LispString)
	return ok && L.S == other.S
}

func (L *LispString) Call(ctx context.Context, w *World, n Node) (Node, error) {
	compile, err := L.Eval(ctx, w)
	if err != nil {
		return nil, err
	}
	f, ok := compile.(Callable)
	if !ok {
		return nil, ErrExpectedFunction
	}
	return f.Call(ctx, w, n)
}
