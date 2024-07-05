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

var macroClass = registerNewBuiltInClass[*_Macro]("<macro>")

func (*_Macro) ClassOf() Class {
	return macroClass
}

type _JoinedForm []Node

var joinedFormClass = registerNewBuiltInClass[_JoinedForm]("<joined-form>")

func (_JoinedForm) ClassOf() Class {
	return joinedFormClass
}

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

func cmdLambdaMacro(ctx context.Context, w *World, n Node) (Node, error) {
	v, err := lambdaMacro(ctx, w, n)
	if err != nil {
		return nil, err
	}
	return FunctionRef{value: v}, nil
}

func lambdaMacro(ctx context.Context, w *World, n Node) (*_Macro, error) {
	p, err := getParameterList(n)
	if err != nil {
		return nil, err
	}
	code := p.code
	if c, err := expandMacroInList(ctx, w, code); err == nil {
		code = c
	} else {
		println(err.Error())
	}
	return &_Macro{
		param:   p.param,
		code:    code,
		rest:    p.rest,
		lexical: w,
	}, nil
}

func cmdDefMacro(ctx context.Context, w *World, n Node) (Node, error) {
	cons, ok := n.(*Cons)
	if !ok {
		return nil, ErrExpectedCons
	}
	macroName, err := ExpectSymbol(cons.Car)
	if err != nil {
		return nil, err
	}
	value, err := lambdaMacro(ctx, w, cons.Cdr)
	if err != nil {
		return nil, err
	}
	w.defun.Set(macroName, value)
	if w.macro == nil {
		w.macro = make(map[Symbol]*_Macro)
	}
	w.macro[macroName] = value
	return macroName, nil
}

func funMacroExpand(ctx context.Context, w *World, args []Node) (Node, error) {
	_name, param, err := Shift(args[0])
	if err != nil {
		return nil, err
	}
	name, err := ExpectSymbol(_name)
	if err != nil {
		return nil, err
	}
	macro, err := w.GetFunc(name)
	if err != nil {
		return nil, err
	}
	if L, ok := macro.(*LispString); ok {
		_macro, err := L.Eval(ctx, w)
		if err != nil {
			return nil, err
		}
		macro, err = ExpectFunction(_macro)
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
