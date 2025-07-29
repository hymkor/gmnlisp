package gmnlisp

import (
	"context"
	"errors"
	"fmt"
)

type _ErrEarlyReturns struct {
	value Node
	tag   blockTagType
}

func (e *_ErrEarlyReturns) Error() string {
	return "Unexpected (return-from)"
}

func cmdReturnFrom(ctx context.Context, w *World, args Node) (Node, error) {
	symbolNode, args, err := Shift(args)
	if err != nil {
		return nil, err
	}
	value, args, err := w.ShiftAndEvalCar(ctx, args)
	if err != nil {
		return nil, err
	}
	if IsSome(args) {
		return raiseProgramError(ctx, w, ErrTooManyArguments)
	}
	symbol, err := ExpectSymbol(ctx, w, symbolNode)
	if err != nil {
		return nil, err
	}
	for p := w; p != nil; p = p.parent {
		if bt, ok := p.aux.(*blockTagType); ok && bt.key.Equals(symbol, STRICT) {
			if _, ok := w.blockName[bt.serial]; ok {
				return nil, &_ErrEarlyReturns{value: value, tag: *bt}
			}
		}
	}
	return raiseControlError(ctx, w, fmt.Errorf("block-tag '%s' not found", symbol.String()))
}

func Progn(ctx context.Context, w *World, n Node) (value Node, err error) {
	value = Null
	for IsSome(n) {
		value, n, err = w.ShiftAndEvalCar(ctx, n)
		if err != nil {
			return nil, err
		}
	}
	return
}

func cmdProgn(ctx context.Context, w *World, c Node) (Node, error) {
	return Progn(ctx, w, c)
}

type blockTagType struct {
	key    Symbol
	serial int
}

var blockTagCount = 0

func newBlockTag(sym Symbol) *blockTagType {
	blockTagCount++
	return &blockTagType{key: sym, serial: blockTagCount}
}

func cmdBlock(ctx context.Context, w *World, node Node) (Node, error) {
	nameNode, statements, err := Shift(node)
	if err != nil {
		return nil, err
	}
	nameSymbol, err := ExpectSymbol(ctx, w, nameNode)
	if err != nil {
		return nil, err
	}

	bt := newBlockTag(nameSymbol)
	newWorld := &World{
		parent: w,
		aux:    bt,
		shared: w.shared,
	}
	if w.blockName == nil {
		w.blockName = make(map[int]struct{})
	}
	if _, ok := w.blockName[bt.serial]; !ok {
		defer delete(w.blockName, bt.serial)
		w.blockName[bt.serial] = struct{}{}
	}
	rv, err := Progn(ctx, newWorld, statements)

	var errEarlyReturns *_ErrEarlyReturns
	if errors.As(err, &errEarlyReturns) && bt.serial == errEarlyReturns.tag.serial && bt.key == errEarlyReturns.tag.key {
		return errEarlyReturns.value, nil
	}
	return rv, err
}

type _ErrThrown struct {
	Value   Node
	TagForm Node
}

func (e *_ErrThrown) Error() string {
	return fmt.Sprintf("Thrown tag-form %#v was not caught", e.TagForm.String())
}
func cmdCatch(ctx context.Context, w *World, node Node) (Node, error) {
	// from ISLisp
	tagForm, statements, err := w.ShiftAndEvalCar(ctx, node)
	if err != nil {
		return nil, err
	}
	if _, ok := tagForm.(Integer); ok {
		return raiseProgramError(ctx, w, errors.New("Can not use an integer as tag-form"))
	}
	if _, ok := tagForm.(Float); ok {
		return raiseProgramError(ctx, w, errors.New("Can not use an float as tag-form"))
	}
	if _, ok := tagForm.(String); ok {
		return raiseProgramError(ctx, w, errors.New("Can not use an string as tag-form"))
	}
	if _, ok := tagForm.(Rune); ok {
		return raiseProgramError(ctx, w, errors.New("Can not use an character as tag-form"))
	}

	if w.catchTag == nil {
		w.catchTag = make(map[Node]struct{})
	}
	if _, ok := w.catchTag[tagForm]; !ok {
		w.catchTag[tagForm] = struct{}{}
		defer delete(w.catchTag, tagForm)
	}

	var errThrown *_ErrThrown
	rv, err := Progn(ctx, w, statements)
	if errors.As(err, &errThrown) && errThrown.TagForm.Equals(tagForm, EQUALP) {
		return errThrown.Value, nil
	}
	return rv, err
}

func funThrow(ctx context.Context, w *World, tagForm, value Node) (Node, error) {
	if w.catchTag == nil {
		w.catchTag = make(map[Node]struct{})
	}
	if _, ok := w.catchTag[tagForm]; !ok {
		return raiseControlError(ctx, w, fmt.Errorf("catch-tag '%s' not found", tagForm.String()))
	}
	return nil, &_ErrThrown{TagForm: tagForm, Value: value}
}

func cmdCond(ctx context.Context, w *World, list Node) (Node, error) {
	return cmdCondWithTailRecOpt(ctx, w, list, nulSymbol)
}

func cmdCondWithTailRecOpt(ctx context.Context, w *World, list Node, currFunc Symbol) (Node, error) {
	for IsSome(list) {
		var condAndAct Node
		var err error

		condAndAct, list, err = Shift(list)
		if err != nil {
			return nil, err
		}
		cond, act, err := w.ShiftAndEvalCar(ctx, condAndAct)
		if err != nil {
			if errors.Is(err, ErrTooFewArguments) {
				return raiseError(ctx, w, errors.New("requires cons"))
			}
			return nil, err
		}
		if IsSome(cond) {
			if IsNone(act) {
				return cond, nil
			}
			return prognWithTailRecOpt(ctx, w, act, currFunc)
		}
	}
	return Null, nil
}

func cmdCase(ctx context.Context, w *World, list Node) (Node, error) {
	var swValue Node
	var err error

	swValue, list, err = w.ShiftAndEvalCar(ctx, list)
	if err != nil {
		return nil, err
	}
	for IsSome(list) {
		var caseAndAct Node
		var err error

		caseAndAct, list, err = Shift(list)
		if err != nil {
			return nil, err
		}
		cons, err := ExpectClass[*Cons](ctx, w, caseAndAct)
		if err != nil {
			return nil, err
		}
		caseValue := cons.Car
		act := cons.Cdr
		if cons, ok := caseValue.(*Cons); ok {
			var list Node = cons
			for IsSome(list) {
				var _caseValue Node
				_caseValue, list, err = Shift(list)
				if err != nil {
					return nil, err
				}
				if swValue.Equals(_caseValue, EQUALP) {
					return Progn(ctx, w, act)
				}
			}
		} else if caseValue.Equals(True, STRICT) {
			if IsSome(list) {
				return raiseProgramError(ctx, w, errors.New("Syntax error"))
			}
			return Progn(ctx, w, act)
		} else {
			return raiseProgramError(ctx, w, errors.New("Syntax error"))
		}
	}
	return Null, nil
}

func cmdIf(ctx context.Context, w *World, params Node) (Node, error) {
	return cmdIfWithTailRecOpt(ctx, w, params, _Symbol(-1))
}

func cmdIfWithTailRecOpt(ctx context.Context, w *World, params Node, tailOptSym Symbol) (Node, error) {
	cond, params, err := w.ShiftAndEvalCar(ctx, params)
	if err != nil {
		return nil, err
	}
	thenClause, params, err := Shift(params)
	if err != nil {
		return nil, err
	}
	var elseClause Node = Null
	if IsSome(params) {
		elseClause, params, err = Shift(params)
		if err != nil {
			return nil, err
		}
		if IsSome(params) {
			return raiseProgramError(ctx, w, ErrTooManyArguments)
		}
	}
	if IsSome(cond) {
		return evalWithTailRecOpt(ctx, w, thenClause, tailOptSym)
	} else if IsSome(elseClause) {
		return evalWithTailRecOpt(ctx, w, elseClause, tailOptSym)
	} else {
		return Null, nil
	}
}

func cmdWhile(ctx context.Context, w *World, n Node) (Node, error) {
	// from autolisp, ISLisp
	cond, statements, err := Shift(n)
	if err != nil {
		return nil, err
	}
	var last Node = Null
	for {
		if err := checkContext(ctx); err != nil {
			return nil, err
		}
		cont, err := w.Eval(ctx, cond)
		if err != nil {
			return nil, err
		}
		if IsNone(cont) {
			return last, nil
		}
		last, err = Progn(ctx, w, statements)
		if err != nil {
			return nil, err
		}
	}
}

func funQuit(context.Context, *World) (Node, error) {
	return Null, ErrQuit
}

func funAbort(context.Context, *World) (Node, error) {
	return Null, ErrAbort
}

func cmdUnwindProtect(ctx context.Context, w *World, list Node) (Node, error) {
	var formErr error
	var value Node

	value, list, formErr = w.ShiftAndEvalCar(ctx, list)
	if IsSome(list) {
		var err error
		_, err = Progn(ctx, w, list)
		if err != nil {
			var e1 *_ErrEarlyReturns // block & return-from
			var e2 *_ErrThrown       // catch & throw
			var e3 *_ErrTagBody      // tagbody & go
			if errors.As(err, &e1) || errors.As(err, &e2) || errors.As(err, &e3) {
				return raiseControlError(ctx, w, errors.New("can not escape from cleanup-form"))
			}
			return nil, err
		}
	}
	if formErr != nil {
		return nil, formErr
	}
	return value, nil
}

type _ErrTagBody struct {
	tag Symbol
}

func (t *_ErrTagBody) Error() string {
	return fmt.Sprintf("tag: %s not found", t.tag)
}

func cmdGo(ctx context.Context, w *World, args Node) (Node, error) {
	tag, args, err := Shift(args)
	if err != nil {
		return nil, err
	}
	if IsSome(args) {
		return nil, ErrTooManyArguments
	}
	symbol, err := ExpectSymbol(ctx, w, tag)
	if err != nil {
		return nil, err
	}
	if w.goTag == nil {
		w.goTag = make(map[Symbol]struct{})
	}
	if _, ok := w.goTag[symbol]; !ok {
		return raiseControlError(ctx, w, fmt.Errorf("go-tag: %s not found", symbol.String()))
	}
	return Null, &_ErrTagBody{tag: symbol}
}

func cmdTagBody(ctx context.Context, w *World, args Node) (Node, error) {
	tag := map[Symbol]Node{}

	if w.goTag == nil {
		w.goTag = make(map[Symbol]struct{})
	}
	for _args := args; IsSome(_args); {
		var current Node
		var err error

		current, _args, err = Shift(_args)
		if err != nil {
			return nil, err
		}
		if symbol, ok := current.(Symbol); ok {
			if _, ok := w.goTag[symbol]; !ok {
				w.goTag[symbol] = struct{}{}
				defer delete(w.goTag, symbol)
			}
		}
	}
mainloop:
	for IsSome(args) {
		var current Node
		var err error

		current, args, err = Shift(args)
		if err != nil {
			return nil, err
		}
		if symbol, ok := current.(Symbol); ok {
			tag[symbol] = args
			continue
		}
		_, err = w.Eval(ctx, current)
		if err == nil {
			continue
		}
		var t *_ErrTagBody
		if !errors.As(err, &t) {
			return nil, err
		}
		if next, ok := tag[t.tag]; ok {
			args = next
			continue
		}
		for IsSome(args) {
			current, args, err = Shift(args)
			if err != nil {
				return nil, err
			}
			symbol, ok := current.(Symbol)
			if !ok {
				continue
			}
			tag[symbol] = args
			if symbol == t.tag {
				continue mainloop
			}
		}
		return nil, err
	}
	return Null, nil
}

func cmdIgnoreErrors(ctx context.Context, w *World, n Node) (Node, error) {
	val, err := Progn(ctx, w, n)
	if err != nil {
		return Null, nil
	}
	return val, nil
}
