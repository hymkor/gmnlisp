package gmnlisp

import (
	"errors"
	"fmt"
	"io"
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

func (nl *_Lambda) PrintTo(w io.Writer, m PrintMode) {
	io.WriteString(w, "(lambda (")
	dem := ""
	for _, name := range nl.param {
		io.WriteString(w, dem)
		io.WriteString(w, name)
		dem = " "
	}
	io.WriteString(w, ") ")
	if cons, ok := nl.code.(*Cons); ok {
		cons.writeToWithoutKakko(w, m)
	} else {
		nl.code.PrintTo(w, m)
	}
	io.WriteString(w, ")")
}

func (nl *_Lambda) Call(w *World, n Node) (Node, error) {
	globals := map[string]Node{}
	foundSlash := false
	for _, name := range nl.param {
		if name == "/" {
			foundSlash = true
			continue
		}
		if foundSlash {
			globals[name] = Null
			continue
		}
		cons, ok := n.(*Cons)
		if !ok {
			return nil, ErrTooFewArguments
		}
		var err error

		globals[name], err = cons.GetCar().Eval(w)
		if err != nil {
			return nil, err
		}
		n = cons.GetCdr()
	}
	if HasValue(n) {
		return nil, ErrTooManyArguments
	}
	newWorld := &World{globals: globals, parent: nl.lexical}

	var errEarlyReturns *ErrEarlyReturns

	result, err := progn(newWorld, nl.code)
	if errors.As(err, &errEarlyReturns) && errEarlyReturns.Name == string(nl.name) {
		return errEarlyReturns.Value, nil
	}
	return result, err
}

func (nl *_Lambda) Eval(*World) (Node, error) {
	return nil, ErrIllegalFunctionCall
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
	fc, node, err := w.shiftAndEvalCar(node)
	if err != nil {
		return nil, err
	}
	f, err := fc.Eval(w)
	if err != nil {
		return nil, err
	}
	_f, ok := f.(_Callable)
	if !ok {
		return nil, ErrExpectedFunction
	}
	return _f.Call(w, node)
}
