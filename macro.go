package gmnlisp

import (
	"errors"
	"fmt"
	"io"
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
	if macroParam, ok := n.(_PlaceHolder); ok {
		if result, ok := table[string(macroParam)]; ok {
			return result
		}
	}
	return n
}

func (m *_Macro) expand(n Node) (Node, error) {
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
	return replaceMacro(m.code, replaceTbl), nil
}

func (m *_Macro) Call(w *World, n Node) (Node, error) {
	code, err := m.expand(n)
	if err != nil {
		return nil, err
	}
	return code.Eval(w)
}

type _PlaceHolder string

func (mp _PlaceHolder) PrintTo(w io.Writer) {
	fmt.Fprintf(w, "$(%s)", string(mp))
}

func (mp _PlaceHolder) Equals(n Node) bool {
	if _n, ok := n.(_PlaceHolder); ok {
		return mp == _n
	}
	return false
}

func (mp _PlaceHolder) Eval(*World) (Node, error) {
	return mp, nil
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
	param, code, err := getParameterList(cons.Cdr)
	if err != nil {
		return nil, err
	}

	globals := map[string]Node{}
	for _, name := range param {
		globals[name] = _PlaceHolder(name)
	}
	nw := w.newWorld(globals, w.scope)

	code, err = progn(nw, code)
	if err != nil {
		return nil, err
	}
	// code.PrintTo(os.Stdout)

	value := &_Macro{
		param: param,
		code:  code,
	}
	w.Set(string(macroName), value)
	return value, nil
}

func cmdMacroExpand(w *World, n Node) (Node, error) {
	var err error
	n, _, err = w.shiftAndEvalCar(n)
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
	node, err := w.Get(string(macroName))
	if err != nil {
		return nil, err
	}
	macro, ok := node.(*_Macro)
	if !ok {
		return nil, errors.New("Expected Macro")
	}
	return macro.expand(cons.Cdr)
}
