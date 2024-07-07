package gmnlisp

import (
	"context"
	"fmt"
	"io"
)

type FunctionRef struct {
	value Callable
}

var functionRefClassObject = newBuiltInClass[FunctionRef]("<function>")

func (f FunctionRef) ClassOf() Class {
	return functionRefClassObject
}

func (f FunctionRef) Equals(other Node, mode EqlMode) bool {
	_other, ok := other.(FunctionRef)
	if !ok {
		return false
	}
	return f.value == _other.value
}

func (f FunctionRef) PrintTo(w io.Writer, _ PrintMode) (int, error) {
	return fmt.Fprintf(w, "(function to %#v)", f.value)
}

func (f FunctionRef) String() string {
	return fmt.Sprintf("(function to %#v)", f.value)
}

func (f FunctionRef) GoString() string {
	return fmt.Sprintf("function to %#v)", f.value)
}

func (f FunctionRef) Eval(ctx context.Context, w *World) (Node, error) {
	return f, nil
}

func cmdFunction(ctx context.Context, w *World, node Node) (Node, error) {
	_symbol, node, err := Shift(node)
	if err != nil {
		return nil, err
	}
	if IsSome(node) {
		return nil, ErrTooManyArguments
	}
	symbol, err := ExpectClass[Symbol](ctx, w, _symbol)
	if err != nil {
		return nil, err
	}
	f, err := w.GetFunc(symbol)
	if err != nil {
		return nil, err
	}
	// The consequences are undefined if the function-name names a macro or special form.
	//   https://islisp-dev.github.io/ISLispHyperDraft/islisp-v23.html#s_function
	if _, ok := f.(*_Macro); ok {
		return nil, fmt.Errorf("macro forms can be avaliable on (function): %#v", symbol)
	}
	if _, ok := f.(SpecialF); ok {
		return nil, fmt.Errorf("special forms can be avaliable on (function): %#v", symbol)
	}
	return FunctionRef{value: f}, nil
}

func ExpectFunction(ctx context.Context, w *World, value Node) (Callable, error) {
	v, err := ExpectClass[FunctionRef](ctx, w, value)
	if err != nil {
		return nil, err
	}
	return v.value, nil
}

func cmdFlet(ctx context.Context, w *World, list Node) (Node, error) {
	flist, list, err := Shift(list)
	if err != nil {
		return nil, err
	}
	lexical := Functions{}
	for HasValue(flist) {
		var flist1 Node
		flist1, flist, err = Shift(flist)
		if err != nil {
			return nil, err
		}
		var name Node
		name, flist1, err = Shift(flist1)
		if err != nil {
			return nil, err
		}
		symbol, err := ExpectClass[Symbol](ctx, w, name)
		if err != nil {
			return nil, err
		}
		lambda, err := newLambda(ctx, w, flist1, symbol)
		if err != nil {
			return nil, err
		}
		lexical[symbol] = lambda
	}
	nw := w.Flet(lexical)
	return Progn(ctx, nw, list)
}

func cmdLabels(ctx context.Context, w *World, list Node) (Node, error) {
	flist, list, err := Shift(list)
	if err != nil {
		return nil, err
	}
	lexical := Functions{}
	nw := w.Flet(lexical)
	for HasValue(flist) {
		var flist1 Node
		flist1, flist, err = Shift(flist)
		if err != nil {
			return nil, err
		}
		var name Node
		name, flist1, err = Shift(flist1)
		if err != nil {
			return nil, err
		}
		symbol, err := ExpectClass[Symbol](ctx, w, name)
		if err != nil {
			return nil, err
		}
		lambda, err := newLambda(ctx, nw, flist1, symbol)
		if err != nil {
			return nil, err
		}
		lexical[symbol] = lambda
	}
	return Progn(ctx, nw, list)
}
