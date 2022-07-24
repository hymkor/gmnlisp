package gmnlisp

import (
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

func cmdLambda(w *World, node Node) (Node, error) {
	return newLambda(w, node, "")
}

func getParameterList(node Node) ([]string, Node, error) {
	cons, ok := node.(*Cons)
	if !ok {
		return nil, nil, fmt.Errorf("%w for parameter list", ErrExpectedCons)
	}
	params := []string{}
	if err := forEachList(cons.Car, func(n Node) error {
		name, ok := n.(Symbol)
		if !ok {
			return ErrExpectedSymbol
		}
		params = append(params, string(name))
		return nil
	}); err != nil {
		return nil, nil, err
	}
	return params, cons.GetCdr(), nil
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

func (L *_Lambda) Call(w *World, n Node) (Node, error) {
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
		value, n, err = w.shiftAndEvalCar(n)
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

	result, err := progn(newWorld, L.code)
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

func (L *_Lambda) Eval(*World) (Node, error) {
	return L, nil
}

func (*_Lambda) Equals(Node, EqlMode) bool {
	return false
}

func cmdDefun(w *World, node Node) (Node, error) {
	cons, ok := node.(*Cons)
	if !ok {
		return nil, ErrExpectedCons
	}
	symbol, ok := cons.Car.(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}
	name := string(symbol)

	lambda, err := newLambda(w, cons.Cdr, name)
	if err != nil {
		return nil, err
	}
	w.Set(name, lambda)
	return symbol, nil
}

func cmdFunCall(w *World, node Node) (Node, error) {
	f, node, err := w.shiftAndEvalCar(node)
	if err != nil {
		return nil, err
	}
	_f, ok := f.(_Callable)
	if !ok {
		return nil, ErrExpectedFunction
	}
	return _f.Call(w, node)
}

type _Callable interface {
	Node
	Call(*World, Node) (Node, error)
}

type Function func(*World, Node) (Node, error)

func (Function) PrintTo(w io.Writer, m PrintMode) {
	io.WriteString(w, "buildin function")
}

func (f Function) Eval(_ *World) (Node, error) {
	return f, nil
}

func (f Function) Equals(n Node, m EqlMode) bool {
	return false
}

func (f Function) Call(w *World, n Node) (Node, error) {
	return f(w, n)
}

func cmdFunction(w *World, node Node) (Node, error) {
	var argv [1]Node
	if err := w.evalListAll(node, argv[:]); err != nil {
		return nil, err
	}
	f, ok := argv[0].(_Callable)
	if !ok {
		return nil, ErrExpectedFunction
	}
	return f, nil
}

func cmdTrace(w *World, list Node) (Node, error) {
	if len(trace) > 0 {
		trace = map[string]int{}
	}
	return Null, forEachList(list, func(node Node) error {
		symbol, ok := node.(Symbol)
		if !ok {
			return ErrExpectedSymbol
		}
		trace[string(symbol)] = 0
		return nil
	})
}
