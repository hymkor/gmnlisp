package gmnlisp

import (
	"context"
	"errors"
	"fmt"
	"io"
)

func _macroQuote(name string, list Node) (Node, error) {
	cons, ok := list.(*Cons)
	if !ok {
		return nil, fmt.Errorf("%s: %w", name, ErrTooFewArguments)
	}
	quoted, err := macroQuote(cons.Car)
	if err != nil {
		return nil, err
	}
	rest, err := macroQuote(cons.Cdr)
	if err != nil {
		return nil, err
	}
	return &Cons{
		Car: &Cons{
			Car: Symbol(name),
			Cdr: &Cons{
				Car: quoted,
				Cdr: nil,
			},
		},
		Cdr: rest,
	}, nil
}

func macroQuote(node Node) (Node, error) {
	cons, ok := node.(*Cons)
	if !ok {
		return node, nil
	}
	// normal pair

	if car := cons.GetCar(); car.Equals(Symbol("'"), EQUAL) {
		return _macroQuote("quote", cons.Cdr)
	} else if car.Equals(Symbol("#'"), EQUAL) {
		return _macroQuote("function", cons.Cdr)
	}
	car, err := macroQuote(cons.Car)
	if err != nil {
		return nil, err
	}
	cdr, err := macroQuote(cons.Cdr)
	if err != nil {
		return nil, err
	}
	return &Cons{Car: car, Cdr: cdr}, nil
}

type _Macro struct {
	_Dummy
	param []Symbol
	code  Node
}

func replaceMacro(n Node, table map[Symbol]Node) Node {
	if cons, ok := n.(*Cons); ok {
		var car Node
		var cdr Node
		if cons.Car != nil {
			car = replaceMacro(cons.Car, table)
		}
		if cons.Cdr != nil {
			cdr = replaceMacro(cons.Cdr, table)
		}
		return &Cons{Car: car, Cdr: cdr}
	}
	if macroParam, ok := n.(_PlaceHolder); ok {
		if result, ok := table[Symbol(macroParam)]; ok {
			return result
		}
	}
	return n
}

func (m *_Macro) expand(n Node) (Node, error) {
	replaceTbl := map[Symbol]Node{}
	for _, name := range m.param {
		if IsNull(n) {
			return nil, ErrTooFewArguments
		}
		cons, ok := n.(*Cons)
		if !ok {
			return nil, ErrExpectedCons
		}
		replaceTbl[name] = cons.Car
		n = cons.Cdr
	}
	if HasValue(n) {
		return nil, ErrTooManyArguments
	}
	return replaceMacro(m.code, replaceTbl), nil
}

func (m *_Macro) Call(ctx context.Context, w *World, n Node) (Node, error) {
	code, err := m.expand(n)
	if err != nil {
		return nil, err
	}
	return code.Eval(ctx, w)
}

type _PlaceHolder string

func (mp _PlaceHolder) PrintTo(w io.Writer, m PrintMode) {
	fmt.Fprintf(w, "$(%s)", string(mp))
}

func (mp _PlaceHolder) Equals(n Node, m EqlMode) bool {
	if _n, ok := n.(_PlaceHolder); ok {
		return mp == _n
	}
	return false
}

func (mp _PlaceHolder) Eval(context.Context, *World) (Node, error) {
	return mp, nil
}

func cmdDefMacro(ctx context.Context, w *World, n Node) (Node, error) {
	cons, ok := n.(*Cons)
	if !ok {
		return nil, ErrExpectedCons
	}
	macroName, ok := cons.Car.(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}
	p, err := getParameterList(cons.Cdr)
	if err != nil {
		return nil, err
	}
	param := p.param
	code := p.code

	globals := map[Symbol]Node{}
	for _, name := range param {
		globals[name] = _PlaceHolder(name)
	}
	nw := &World{globals: globals, parent: w}

	code, err = progn(ctx, nw, code)
	if err != nil {
		return nil, err
	}
	// code.PrintTo(os.Stdout)

	value := &_Macro{
		param: param,
		code:  code,
	}
	w.SetOrDefineParameter(macroName, value)
	return value, nil
}

func cmdMacroExpand(ctx context.Context, w *World, n Node) (Node, error) {
	var err error
	n, _, err = w.shiftAndEvalCar(ctx, n)
	if err != nil {
		return nil, err
	}
	cons, ok := n.(*Cons)
	if !ok {
		return nil, ErrExpectedCons
	}
	macroName, ok := cons.Car.(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}
	node, err := w.Get(macroName)
	if err != nil {
		return nil, err
	}
	macro, ok := node.(*_Macro)
	if !ok {
		return nil, errors.New("Expected Macro")
	}
	return macro.expand(cons.Cdr)
}
