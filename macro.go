package gmnlisp

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"
)

const macro_trace = false

type _Macro struct {
	param   []Symbol
	code    Node
	rest    Symbol
	lexical *World
}

type _JoinedForm []Node

func (j _JoinedForm) PrintTo(w io.Writer, m PrintMode) (int, error) {
	dem := "_JoinedForm("
	n := 0
	for _, e := range j {
		_n, err := io.WriteString(w, dem)
		n += _n
		if err != nil {
			return n, err
		}
		dem = " "

		_n, err = e.PrintTo(w, m)
		n += _n
		if err != nil {
			return n, err
		}
	}
	_n, err := w.Write([]byte{')'})
	n += _n
	return n, err
}

func (j _JoinedForm) Eval(ctx context.Context, w *World) (Node, error) {
	return Null, errors.New("not supported")
}

func (j _JoinedForm) Equals(_other Node, m EqlMode) bool {
	other, ok := _other.(_JoinedForm)
	if !ok {
		return false
	}
	if len(j) != len(other) {
		return false
	}
	for i := range j {
		if j[i].Equals(other[i], m) {
			return false
		}
	}
	return true
}

func expandJoinedForm(n Node) Node {
	if cons, ok := n.(*Cons); ok {
		var cdr Node
		if cons.Cdr != nil {
			cdr = expandJoinedForm(cons.Cdr)
		}
		var car Node
		if cons.Car != nil {
			car = expandJoinedForm(cons.Car)
			if c, ok := car.(_JoinedForm); ok {
				var _cons Node = cdr
				for i := len(c) - 1; i >= 0; i-- {
					_cons = &Cons{Car: expandJoinedForm(c[i]), Cdr: _cons}
				}
				return _cons
			}
		}
		return &Cons{Car: car, Cdr: cdr}
	}
	return n
}

func (m *_Macro) Call(ctx context.Context, w *World, n Node) (Node, error) {
	var err error

	lexical := Variables{}
	for _, name := range m.param {
		lexical[name], n, err = Shift(n)
		if err != nil {
			return nil, err
		}
	}
	lexical[m.rest] = n
	joinedForm := _JoinedForm{}
	for cons, ok := n.(*Cons); ok && HasValue(cons); cons, ok = cons.Cdr.(*Cons) {
		joinedForm = append(joinedForm, cons.Car)
	}
	lexical[NewSymbol("@"+m.rest.String())] = joinedForm

	newWorld := m.lexical.Let(lexical)

	if macro_trace {
		m.code.PrintTo(os.Stderr, PRINT)
		fmt.Fprintln(os.Stderr, "\n----")
	}

	newCode, err := Progn(ctx, newWorld, m.code)
	if err != nil {
		return nil, err
	}

	if macro_trace {
		newCode.PrintTo(os.Stderr, PRINT)
		fmt.Fprintln(os.Stderr, "\n----")
	}

	newCode = expandJoinedForm(newCode)

	if macro_trace {
		newCode.PrintTo(os.Stderr, PRINT)
		fmt.Fprintln(os.Stderr, "\n----")
	}

	return newCode.Eval(ctx, w)
}

func cmdLambaMacro(ctx context.Context, w *World, n Node) (Node, error) {
	p, err := getParameterList(n)
	if err != nil {
		return nil, err
	}
	return &_Macro{
		param:   p.param,
		code:    p.code,
		rest:    p.rest,
		lexical: w,
	}, nil
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
	value, err := cmdLambaMacro(ctx, w, cons.Cdr)
	if err != nil {
		return nil, err
	}
	w.SetOrDefineParameter(macroName, value)
	return macroName, nil
}
