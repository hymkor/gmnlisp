package gmnlisp

import (
	"context"
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
	var wc writeCounter
	for _, e := range j {
		if wc.Try(io.WriteString(w, dem)) {
			return wc.Result()
		}
		dem = " "

		if wc.Try(e.PrintTo(w, m)) {
			return wc.Result()
		}
	}
	wc.Try(w.Write([]byte{')'}))
	return wc.Result()
}

func (j _JoinedForm) Eval(ctx context.Context, w *World) (Node, error) {
	return j, nil
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

func (m *_Macro) expand(ctx context.Context, w *World, n Node) (Node, error) {
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
	for cons, ok := n.(*Cons); ok && IsSome(cons); cons, ok = cons.Cdr.(*Cons) {
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
	return newCode, nil
}

func (m *_Macro) Call(ctx context.Context, w *World, n Node) (Node, error) {
	newCode, err := m.expand(ctx, w, n)
	if err != nil {
		return nil, err
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

func funMacroExpand(ctx context.Context, w *World, args []Node) (Node, error) {
	name, param, err := Shift(args[0])
	if err != nil {
		return nil, err
	}
	macro, err := name.Eval(ctx, w)
	if err != nil {
		return nil, MakeError(err, name)
	}
	if L, ok := macro.(*LispString); ok {
		macro, err = L.Eval(ctx, w)
		if err != nil {
			return nil, err
		}
	}
	m, ok := macro.(*_Macro)
	if !ok {
		return nil, MakeError(ErrExpectedMacro, macro)
	}
	return m.expand(ctx, w, param)
}
