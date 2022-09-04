package common

import (
	"context"

	. "github.com/hymkor/gmnlisp"
)

func Using(w *World) *World {
	defparameter, err := w.Get("defglobal")
	if err != nil {
		panic("not found defglobal")
	}
	return w.Let(Variables{
		"defvar":       SpecialF(cmdDefvar),
		"defparameter": defparameter,
	})
}

func cmdDefvar(ctx context.Context, w *World, list Node) (Node, error) {
	// from CommonLisp
	var symbolNode Node
	var err error

	symbolNode, list, err = Shift(list)
	if err != nil {
		return nil, err
	}
	symbol, ok := symbolNode.(Symbol)
	if !ok {
		return nil, ErrExpectedSymbol
	}
	w.DefineVariable(symbol, func() Node {
		var value Node = Null
		if HasValue(list) {
			value, _, err = w.ShiftAndEvalCar(ctx, list)
		}
		return value
	})
	return symbol, err
}
