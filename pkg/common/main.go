package common

import (
	"context"

	. "github.com/hymkor/gmnlisp"
)

var Functions = Variables{
	NewSymbol("member"):       &KWFunction{C: 2, F: funMember},
	NewSymbol("find"):         &KWFunction{C: 2, F: funFind},
	NewSymbol("position"):     &KWFunction{C: 2, F: funPosition},
	NewSymbol("defparameter"): SpecialF(cmdDefparameter),
	NewSymbol("defvar"):       SpecialF(cmdDefvar),
	NewSymbol("prin1"):        &Function{C: 1, F: funPrin1},
	NewSymbol("princ"):        &Function{C: 1, F: funPrinc},
	NewSymbol("print"):        &Function{C: 1, F: funPrint},
	NewSymbol("terpri"):       &Function{C: -1, F: funTerpri},
	NewSymbol("write"):        &KWFunction{C: 1, F: funWrite},
	NewSymbol("write-line"):   SpecialF(cmdWriteLine),
	NewSymbol("when"):         SpecialF(cmdWhen),
	NewSymbol("unless"):       SpecialF(cmdUnless),
	NewSymbol("nth"):          &LeftValueF{C: 2, F: funNth},
	NewSymbol("nthcdr"):       &LeftValueF{C: 2, F: funNthcdr},
	NewSymbol("cadddr"):       &LeftValueF{C: 1, F: funCadddr},
	NewSymbol("caddr"):        &LeftValueF{C: 1, F: funCaddr},
	NewSymbol("cadr"):         &LeftValueF{C: 1, F: funCadr},
	NewSymbol("cdddr"):        &LeftValueF{C: 1, F: funCdddr},
	NewSymbol("cddr"):         &LeftValueF{C: 1, F: funCddr},
	//NewSymbol("first"):                       &LeftValueF{C: 1, F: funGetCar},
	NewSymbol("second"):         &LeftValueF{C: 1, F: funCadr},
	NewSymbol("third"):          &LeftValueF{C: 1, F: funCaddr},
	NewSymbol("concatenate"):    defConcatenate,
	NewSymbol("dolist"):         SpecialF(cmdDoList),
	NewSymbol("dotimes"):        SpecialF(cmdDoTimes),
	NewSymbol("with-open-file"): SpecialF(cmdWithOpenFile),
}

var defparameter Callable

func cmdDefparameter(ctx context.Context, w *World, list Node) (Node, error) {
	if IsNull(defparameter) {
		_defparameter, err := w.Get(NewSymbol("defglobal"))
		if err != nil {
			return nil, err
		}
		var ok bool
		defparameter, ok = _defparameter.(Callable)
		if !ok {
			return nil, ErrExpectedFunction
		}
	}
	return defparameter.Call(ctx, w, list)
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

func cmdWhen(ctx context.Context, w *World, args Node) (Node, error) {
	cond, args, err := w.ShiftAndEvalCar(ctx, args)
	if err != nil {
		return nil, err
	}
	if IsNull(cond) {
		return Null, nil
	}
	return Progn(ctx, w, args)
}

func cmdUnless(ctx context.Context, w *World, args Node) (Node, error) {
	cond, args, err := w.ShiftAndEvalCar(ctx, args)
	if err != nil {
		return nil, err
	}
	if HasValue(cond) {
		return Null, nil
	}
	return Progn(ctx, w, args)
}
