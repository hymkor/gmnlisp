package gmnlisp

import (
	"context"
	"errors"
	"fmt"
	"io"
)

type _ErrEarlyReturns struct {
	Value Node
	Name  Symbol
}

func (e *_ErrEarlyReturns) Error() string {
	if e.Name == nulSymbol {
		return "Unexpected (return)"
	}
	return fmt.Sprintf("Unexpected (return-from %s)", e.Name)
}

func funReturn(ctx context.Context, w *World, argv []Node) (Node, error) {
	// from CommonLisp
	return nil, &_ErrEarlyReturns{Value: argv[0], Name: nulSymbol}
}

func cmdReturnFrom(ctx context.Context, w *World, n Node) (Node, error) {
	// from CommonLisp
	var argv [2]Node
	if err := ListToArray(n, argv[:]); err != nil {
		return nil, err
	}
	symbol, err := ExpectSymbol(argv[0])
	if err != nil {
		return nil, err
	}
	value, err := argv[1].Eval(ctx, w)
	if err != nil {
		return nil, err
	}
	return nil, &_ErrEarlyReturns{Value: value, Name: symbol}
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

func cmdBlock(ctx context.Context, w *World, node Node) (Node, error) {
	// from CommonLisp
	nameNode, statements, err := Shift(node)
	if err != nil {
		return nil, err
	}
	var nameSymbol Symbol

	if IsSome(nameNode) {
		nameSymbol, err = ExpectSymbol(nameNode)
		if err != nil {
			return nil, err
		}
	} else {
		nameSymbol = nulSymbol
	}

	var errEarlyReturns *_ErrEarlyReturns
	rv, err := Progn(ctx, w, statements)
	if errors.As(err, &errEarlyReturns) && errEarlyReturns.Name == nameSymbol {
		return errEarlyReturns.Value, nil
	}
	return rv, err
}

type _ErrThrown struct {
	Value   Node
	TagForm Node
}

func (e *_ErrThrown) Error() string {
	return fmt.Sprintf("Thrown tag-form %#v was not caught", e.TagForm)
}
func cmdCatch(ctx context.Context, w *World, node Node) (Node, error) {
	// from ISLisp
	tagForm, statements, err := w.ShiftAndEvalCar(ctx, node)
	if err != nil {
		return nil, err
	}

	var errThrown *_ErrThrown
	rv, err := Progn(ctx, w, statements)
	if errors.As(err, &errThrown) && errThrown.TagForm.Equals(tagForm, EQUALP) {
		return errThrown.Value, nil
	}
	return rv, err
}

func funThrow(ctx context.Context, w *World, list []Node) (Node, error) {
	// from ISLisp
	return nil, &_ErrThrown{Value: list[1], TagForm: list[0]}
}

func cmdCond(ctx context.Context, w *World, list Node) (Node, error) {
	return cmdCondWithTailRecOpt(ctx, w, list, -1)
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
			return nil, err
		}
		if IsSome(cond) {
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
		caseValue, act, err := Shift(caseAndAct)
		if err != nil {
			return nil, err
		}
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
			return Progn(ctx, w, act)
		}
	}
	return Null, nil
}

func cmdIf(ctx context.Context, w *World, params Node) (Node, error) {
	return cmdIfWithTailRecOpt(ctx, w, params, -1)
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
			return nil, ErrTooManyArguments
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
		cont, err := cond.Eval(ctx, w)
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

func cmdQuit(context.Context, *World, Node) (Node, error) {
	return Null, ErrQuit
}

func cmdAbort(context.Context, *World, Node) (Node, error) {
	return Null, ErrAbort
}

type ErrorNode struct {
	Value error
}

var errorClass = &_BuiltInClass{
	name: NewSymbol("<error>"),
	instanceP: func(value Node) bool {
		_, ok := value.(error)
		return ok
	},
	create: func() Node {
		panic("the instance of <error> could not be created")
	},
	super: []Class{objectClass},
}

func (*ErrorNode) ClassOf() Class {
	return errorClass
}

func (e *ErrorNode) PrintTo(w io.Writer, m PrintMode) (int, error) {
	return io.WriteString(w, e.Value.Error())
}

func (e *ErrorNode) Eval(context.Context, *World) (Node, error) {
	return e, nil
}

func (e *ErrorNode) Equals(n Node, m EqlMode) bool {
	f, ok := n.(*ErrorNode)
	if !ok {
		return false
	}
	return errors.Is(e.Value, f.Value) || errors.Is(f.Value, e.Value)
}

func cmdUnwindProtect(ctx context.Context, w *World, list Node) (Node, error) {
	var formErr error

	_, list, formErr = w.ShiftAndEvalCar(ctx, list)

	value, err := Progn(ctx, w, list)
	if err != nil {
		return nil, err
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
	tag, _, err := Shift(args)
	if err != nil {
		return nil, err
	}
	symbol, err := ExpectSymbol(tag)
	if err != nil {
		return nil, err
	}
	return Null, &_ErrTagBody{tag: symbol}
}

func cmdTagBody(ctx context.Context, w *World, args Node) (Node, error) {
	tag := map[Symbol]Node{}

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
		_, err = current.Eval(ctx, w)
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
