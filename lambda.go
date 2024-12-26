package gmnlisp

import (
	"context"
	"errors"
	"fmt"
	"io"
	"math"
	"os"
	"strings"
)

type _Lambda struct {
	param   []Symbol
	code    Node
	name    Symbol
	rest    Symbol
	lexical *World
}

func cmdLambda(ctx context.Context, w *World, node Node) (Node, error) {
	value, err := newLambda(ctx, w, node, nulSymbol)
	if err != nil {
		return nil, err
	}
	return FunctionRef{value: value}, nil
}

type _Parameters struct {
	param []Symbol
	rest  Symbol
	code  Node
}

func getParameterList(ctx context.Context, w *World, node Node) (*_Parameters, error) {
	list, code, err := Shift(node)
	if err != nil {
		return nil, err
	}
	dupCheck := make(map[Symbol]struct{})
	params := []Symbol{}
	var rest Symbol = nulSymbol
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
			rest, err = ExpectSymbol(ctx, w, nameNode)
			if err != nil {
				return nil, err
			}
			if _, ok := dupCheck[rest]; ok {
				_, err = raiseError(ctx, w, fmt.Errorf("%s: same name parameter already exists", rest.String()))
				return nil, err
			}
			dupCheck[rest] = struct{}{}
		} else {
			nameSymbol, err := ExpectSymbol(ctx, w, nameNode)
			if err != nil {
				return nil, err
			}
			if _, ok := dupCheck[nameSymbol]; ok {
				_, err = raiseError(ctx, w, fmt.Errorf("%s: same name parameter already exists", nameSymbol.String()))
				return nil, err
			}
			dupCheck[nameSymbol] = struct{}{}
			params = append(params, nameSymbol)
		}

	}
	return &_Parameters{
		param: params,
		rest:  rest,
		code:  code,
	}, nil
}

func expandMacroOne(ctx context.Context, w *World, value Node) Node {
	cons, ok := value.(*Cons)
	if !ok {
		return nil
	}
	symbol, ok := cons.Car.(Symbol)
	if !ok {
		car := expandMacroOne(ctx, w, cons.Car)
		cdr := expandMacroOne(ctx, w, cons.Cdr)
		return &Cons{Car: car, Cdr: cdr}
	}
	macro, ok := w.macro[symbol]
	if !ok {
		f, err := w.GetFunc(symbol)
		if err != nil {
			return nil
		}
		ls, ok := f.(*LispString)
		if !ok {
			return nil
		}
		fm, err := ls.Eval(ctx, w)
		if err != nil {
			return nil
		}
		fmacro, ok := fm.(FunctionRef)
		if !ok {
			return nil
		}
		macro, ok = fmacro.value.(*_Macro)
		if !ok {
			return nil
		}
	}
	expandCode, err := macro.expand(ctx, w, cons.Cdr)
	if err != nil {
		println(err.Error())
		return nil
	}
	if x := expandMacroOne(ctx, w, expandCode); x != nil {
		return x
	}
	return expandCode
}

func expandMacroInList(ctx context.Context, w *World, code Node) (Node, error) {
	var newCode Node
	for IsSome(code) {
		var value Node
		var err error
		value, code, err = Shift(code)
		if err != nil {
			return nil, err
		}
		if c := expandMacroOne(ctx, w, value); c != nil {
			value = c
		}
		newCode = &Cons{Car: value, Cdr: newCode}
	}
	return NReverse(ctx, w, newCode)
}

func newLambda(ctx context.Context, w *World, node Node, blockName Symbol) (Callable, error) {
	p, err := getParameterList(ctx, w, node)
	if err != nil {
		return nil, err
	}
	code := p.code
	if c, err := expandMacroInList(ctx, w, code); err == nil {
		code = c
	} else {
		println(err.Error())
	}
	return &_Lambda{
		param:   p.param,
		code:    code,
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
		if wc.Try(tryPrintTo(w, name, PRINC)) {
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
		if wc.Try(tryPrintTo(w, L.code, m)) {
			return wc.Result()
		}
	}
	wc.Try(io.WriteString(w, ")"))
	return wc.Result()
}

func (t _Lambda) String() string {
	var buffer strings.Builder
	t.PrintTo(&buffer, PRINC)
	return buffer.String()
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
		return raiseProgramError(ctx, w, ErrTooManyArguments)
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
		lexical[L.rest], err = NReverse(ctx, w, values)
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
	if currFunc.Id() < 0 {
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
	evaled, err = NReverse(ctx, w, evaled)
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
	if currFunc.Id() >= 0 {
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
	return w.Eval(ctx, target)
}

func prognWithTailRecOpt(ctx context.Context, w *World, n Node, sym Symbol) (value Node, err error) {
	value = Null
	for IsSome(n) {
		var first Node

		first, n, err = Shift(n)
		if err != nil {
			return nil, err
		}
		if sym.Id() >= 0 && IsNone(n) {
			value, err = evalWithTailRecOpt(ctx, w, first, sym)
		} else {
			value, err = w.Eval(ctx, first)
		}
		if err != nil {
			return nil, err
		}
	}
	return value, nil
}

func cmdDefun(ctx context.Context, w *World, list Node) (Node, error) {
	_symbol, list, err := Shift(list)
	if err != nil {
		return nil, err
	}
	if IsNone(list) {
		return raiseProgramError(ctx, w, ErrTooFewArguments)
	}
	symbol, err := ExpectSymbol(ctx, w, _symbol)
	if err != nil {
		return nil, err
	}
	lambda, err := newLambda(ctx, w, list, symbol)
	if err != nil {
		return nil, err
	}
	if f, ok := w.defun.Get(symbol); ok {
		if _, ok := f.(SpecialF); ok {
			return raiseProgramError(ctx, w, fmt.Errorf("%s: special operator can not be changed", symbol.String()))
		}
	}
	w.defun.Set(symbol, lambda)
	return symbol, nil
}

func cmdFunCall(ctx context.Context, w *World, node Node) (Node, error) {
	f, node, err := w.ShiftAndEvalCar(ctx, node)
	if err != nil {
		return nil, err
	}
	_f, err := ExpectFunction(ctx, w, f)
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
	f, err := ExpectFunction(ctx, w, funcNode)
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
			value, err := w.Eval(ctx, value)
			if err != nil {
				return nil, err
			}
			SeqEach(ctx, w, value, func(n Node) error {
				newargs.Add(ctx, w, Uneval{n})
				return nil
			})
			return f.Call(ctx, w, newargs.Sequence())
		}
		newargs.Add(ctx, w, value)
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

func cmdTrace(ctx context.Context, w *World, list Node) (Node, error) {
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
		symbol, err := ExpectSymbol(ctx, w, symbolNode)
		if err != nil {
			return nil, err
		}
		trace[symbol] = 0
	}
	return Null, nil
}

func raiseControlError(ctx context.Context, w *World, e error) (Node, error) {
	if _, ok := e.(interface{ ClassOf() Class }); ok {
		return nil, e
	}
	condition := ControlError{err: e}
	return callHandler[Node](ctx, w, false, condition)
}

func raiseProgramError(ctx context.Context, w *World, e error) (Node, error) {
	if _, ok := e.(interface{ ClassOf() Class }); ok {
		return nil, e
	}
	condition := ProgramError{err: e}
	return callHandler[Node](ctx, w, false, condition)
}

type Function0 func(context.Context, *World) (Node, error)

func (f Function0) Call(ctx context.Context, w *World, list Node) (Node, error) {
	if IsSome(list) {
		return raiseProgramError(ctx, w, ErrTooManyArguments)
	}
	return f(ctx, w)
}

type Function1 func(context.Context, *World, Node) (Node, error)

func (f Function1) Call(ctx context.Context, w *World, list Node) (Node, error) {
	v, list, err := w.ShiftAndEvalCar(ctx, list)
	if err != nil {
		return nil, err
	}
	if IsSome(list) {
		return raiseProgramError(ctx, w, ErrTooManyArguments)
	}
	return f(ctx, w, v)
}

type Function2 func(context.Context, *World, Node, Node) (Node, error)

func (f Function2) Call(ctx context.Context, w *World, list Node) (Node, error) {
	first, list, err := w.ShiftAndEvalCar(ctx, list)
	if err != nil {
		return nil, err
	}
	second, list, err := w.ShiftAndEvalCar(ctx, list)
	if err != nil {
		return nil, err
	}
	if IsSome(list) {
		return raiseProgramError(ctx, w, ErrTooManyArguments)
	}
	return f(ctx, w, first, second)
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
		return raiseProgramError(ctx, w, ErrTooManyArguments)
	}
	if len(args) < min {
		return raiseProgramError(ctx, w, ErrTooFewArguments)
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
	f, err := ExpectFunction(ctx, w, compile)
	if err != nil {
		return nil, fmt.Errorf("(*LispString) Call: %w", err)
	}
	return f.Call(ctx, w, n)
}

func cmdExpandDefun(ctx context.Context, w *World, n Node) (Node, error) {
	v, n, err := Shift(n)
	if err != nil {
		return nil, err
	}
	if IsSome(n) {
		return nil, ErrTooManyArguments
	}
	symbol, err := ExpectSymbol(ctx, w, v)
	if err != nil {
		return nil, err
	}
	callable, ok := w.defun.Get(symbol)
	if !ok {
		return nil, &_UndefinedEntity{name: symbol, space: symFunction}
	}
	f, ok := callable.(*_Lambda)
	if !ok {
		return nil, &_UndefinedEntity{name: symbol, space: symFunction}
	}
	var param Node
	for i := len(f.param) - 1; i >= 0; i-- {
		param = &Cons{Car: f.param[i], Cdr: param}
	}
	result := &Cons{Car: param, Cdr: f.code}
	result = &Cons{Car: symbol, Cdr: result}
	result = &Cons{Car: NewSymbol("defun"), Cdr: result}
	fmt.Fprintln(w.stdout, result.GoString())
	return Null, nil
}
