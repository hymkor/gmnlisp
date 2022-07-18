package gmnlisp

import (
	"fmt"
)

func macroQuote(node Node) (Node, error) {
	cons, ok := node.(*Cons)
	if !ok {
		return node, nil
	}
	// normal pair
	if !cons.GetCar().Equals(Symbol("'")) {
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

	// Find single quotation mark
	cons, ok = cons.Cdr.(*Cons)
	if !ok {
		return nil, fmt.Errorf("quote: %w", ErrTooFewArguments)
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
			Car: Symbol("quote"),
			Cdr: &Cons{
				Car: quoted,
				Cdr: nil,
			},
		},
		Cdr: rest,
	}, nil
}

type _Macro struct {
	_Dummy
	param []string
	code  Node
}

func replaceMacro(n Node, table map[string]Node) Node {
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
	if symbol, ok := n.(Symbol); ok {
		if result, ok := table[string(symbol)]; ok {
			return result
		}
	}
	return n
}

func (m *_Macro) Call(w *World, n Node) (Node, error) {
	replaceTbl := map[string]Node{}
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
	resultCode := replaceMacro(m.code, replaceTbl)
	code, err := progn(w, resultCode)
	if err != nil {
		return nil, err
	}
	return code.Eval(w)
}

func cmdDefMacro(w *World, n Node) (Node, error) {
	cons, ok := n.(*Cons)
	if !ok {
		return nil, ErrExpectedCons
	}
	macroName, ok := cons.Car.(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}
	cons, ok = cons.Cdr.(*Cons)
	if !ok {
		return nil, ErrExpectedCons
	}
	param := []string{}
	err := forEachList(cons.Car, func(p Node) error {
		symbol, ok := p.(Symbol)
		if !ok {
			return ErrExpectedSymbol
		}
		param = append(param, string(symbol))
		return nil
	})
	if err != nil {
		return nil, err
	}
	value := &_Macro{
		param: param,
		code:  cons.Cdr,
	}
	w.Set(string(macroName), value)
	return value, nil
}
