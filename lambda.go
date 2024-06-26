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
	value, err := newLambda(w, node, nulSymbol)
	return FunctionRef{value: value}, err
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
	for IsSome(list) {
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
				return nil, MakeError(ErrExpectedSymbol, nameNode)
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

func newLambda(w *World, node Node, blockName Symbol) (Callable, error) {
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
	var wc writeCounter
	if wc.Try(io.WriteString(w, "(lambda (")) {
		return wc.Result()
	}
	dem := ""
	for _, name := range L.param {
		if wc.Try(io.WriteString(w, dem)) {
			return wc.Result()
		}
		if wc.Try(name.PrintTo(w, PRINC)) {
			return wc.Result()
		}
		dem = " "
	}
	if wc.Try(io.WriteString(w, ") ")) {
		return wc.Result()
	}

	if cons, ok := L.code.(*Cons); ok {
		if wc.Try(cons.writeToWithoutKakko(w, m)) {
			return wc.Result()
		}
	} else {
		if wc.Try(L.code.PrintTo(w, m)) {
			return wc.Result()
		}
	}
	wc.Try(io.WriteString(w, ")"))
	return wc.Result()
}

var trace = map[Symbol]int{}

type _ErrTailRecOpt struct {
	params Node
}

func (*_ErrTailRecOpt) Error() string {
	return "_ErrTailRecOpt was not catched."
}

func (L *_Lambda) Call(ctx context.Context, w *World, n Node) (Node, error) {
	if err := checkContext(ctx); err != nil {
		// return nil,fmt.Errorf("%w\n\t%s",err,L.name)
		return nil, err
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
			// return nil,fmt.Errorf("%w\n\t%s",err,L.name)
			return nil, err
		}
		lexical[name] = value
		if traceDo {
			fmt.Fprintf(os.Stderr, " %#v", value)
		}
	}
	if traceDo {
		fmt.Fprintln(os.Stderr, ")]")
	}

	if IsSome(n) && L.rest == nulSymbol {
		// return nil, fmt.Errorf("%s: %w", L.name, ErrTooManyArguments)
		return nil, ErrTooManyArguments
	}
	if L.rest != nulSymbol {
		var values Node = nil
		for IsSome(n) {
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

	var result Node
	var err error
	for {
		newWorld := L.lexical.Let(lexical)
		result, err = prognWithTailRecOpt(ctx, newWorld, L.code, L.name)

		var errTailRecOpt *_ErrTailRecOpt
		if !errors.As(err, &errTailRecOpt) {
			break
		}
		n = errTailRecOpt.params
		foundSlash = false
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
			value, n, err = Shift(n)
			if err != nil {
				// return nil,fmt.Errorf("%w\n\t%s",err,L.name)
				return nil, err
			}
			lexical[name] = value
		}
	}
	var errEarlyReturns *_ErrEarlyReturns
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
		// return nil, fmt.Errorf("%s: %w", L.name, err)
		return nil, err
	}
	return result, nil
}

// If target.Car is current function symbol, then make and return an instance of _ErrTailRecOpt
func testCarIsCurrFunc(ctx context.Context, w *World, target Node, currFunc Symbol) (err error) {
	if currFunc < 0 {
		return nil
	}
	cons, ok := target.(*Cons)
	if !ok || IsNone(cons.Car) || !cons.Car.Equals(currFunc, EQUAL) {
		return nil
	}

	// Tail call optimization
	var params Node = cons.Cdr
	var evaled Node
	for IsSome(params) {
		var value Node
		value, params, err = w.ShiftAndEvalCar(ctx, params)
		if err != nil {
			return err
		}
		evaled = &Cons{Car: value, Cdr: evaled}
	}
	evaled, err = NReverse(evaled)
	if err != nil {
		panic(err.Error())
	}
	return &_ErrTailRecOpt{params: evaled}
}

var (
	symIf    = NewSymbol("if")
	symLet   = NewSymbol("let")
	symLetX  = NewSymbol("let*")
	symProgn = NewSymbol("progn")
	symCond  = NewSymbol("cond")
)

// Evaluate the target considering the tail call optimization.
func evalWithTailRecOpt(ctx context.Context, w *World, target Node, currFunc Symbol) (Node, error) {
	if currFunc >= 0 {
		if err := testCarIsCurrFunc(ctx, w, target, currFunc); err != nil {
			return nil, err
		}
		if cons, ok := target.(*Cons); ok {
			if symIf.Equals(cons.Car, EQUAL) {
				return cmdIfWithTailRecOpt(ctx, w, cons.Cdr, currFunc)
			}
			if symLet.Equals(cons.Car, EQUAL) {
				return cmdLetWithTailRecOpt(ctx, w, cons.Cdr, currFunc)
			}
			if symLetX.Equals(cons.Car, EQUAL) {
				return cmdLetXWithTailRecOpt(ctx, w, cons.Cdr, currFunc)
			}
			if symProgn.Equals(cons.Car, EQUAL) {
				return prognWithTailRecOpt(ctx, w, cons.Cdr, currFunc)
			}
			if symCond.Equals(cons.Car, EQUAL) {
				return cmdCondWithTailRecOpt(ctx, w, cons.Cdr, currFunc)
			}
		}
	}
	return target.Eval(ctx, w)
}

func prognWithTailRecOpt(ctx context.Context, w *World, n Node, sym Symbol) (value Node, err error) {
	value = Null
	for IsSome(n) {
		var first Node

		first, n, err = Shift(n)
		if err != nil {
			return nil, err
		}
		if sym >= 0 && IsNone(n) {
			value, err = evalWithTailRecOpt(ctx, w, first, sym)
		} else {
			value, err = first.Eval(ctx, w)
		}
		if err != nil {
			return nil, err
		}
	}
	return value, nil
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
	w.defun.Set(symbol, lambda)
	return symbol, nil
}

func cmdFunCall(ctx context.Context, w *World, node Node) (Node, error) {
	f, node, err := w.ShiftAndEvalCar(ctx, node)
	if err != nil {
		return nil, err
	}
	_f, err := ExpectFunction(f)
	if err != nil {
		return nil, err
	}
	return _f.Call(ctx, w, node)
}

func cmdApply(ctx context.Context, w *World, list Node) (Node, error) {
	funcNode, list, err := w.ShiftAndEvalCar(ctx, list)
	if err != nil {
		return nil, err
	}
	f, err := ExpectFunction(funcNode)
	if err != nil {
		return nil, err
	}
	var newargs ListBuilder
	for {
		var value Node

		value, list, err = Shift(list)
		if err != nil {
			return nil, err
		}
		if IsNone(list) {
			// value is the last argument = array
			value, err := value.Eval(ctx, w)
			if err != nil {
				return nil, err
			}
			SeqEach(value, func(n Node) error {
				newargs.Add(Uneval{n})
				return nil
			})
			return f.Call(ctx, w, newargs.Sequence())
		}
		newargs.Add(value)
	}
}

type Callable interface {
	Call(context.Context, *World, Node) (Node, error)
}

type SpecialF func(context.Context, *World, Node) (Node, error)

func (f SpecialF) Call(ctx context.Context, w *World, n Node) (Node, error) {
	if err := checkContext(ctx); err != nil {
		return nil, err
	}
	return f(ctx, w, n)
}

func cmdTrace(_ context.Context, _ *World, list Node) (Node, error) {
	// from CommonLisp
	if len(trace) > 0 {
		trace = map[Symbol]int{}
	}
	for IsSome(list) {
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

func (f *Function) Call(ctx context.Context, w *World, list Node) (Node, error) {
	if err := checkContext(ctx); err != nil {
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
	for IsSome(list) {
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

func (L *LispString) Call(ctx context.Context, w *World, n Node) (Node, error) {
	compile, err := L.Eval(ctx, w)
	if err != nil {
		return nil, err
	}
	f, err := ExpectFunction(compile)
	if err != nil {
		return nil, fmt.Errorf("(*LispString) Call: %w", err)
	}
	return f.Call(ctx, w, n)
}
