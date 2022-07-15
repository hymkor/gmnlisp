package gmnlisp

import (
	"fmt"
	"io"
	"os"
)

type _Scope struct {
	parent  *_Scope
	globals map[string]Node
}

type World struct {
	Stdout io.Writer
	scope  *_Scope
}

func (w *World) Get(name string) (Node, error) {
	p := w.scope
	for p != nil {
		if value, ok := p.globals[name]; ok {
			return value, nil
		}
		p = p.parent
	}
	return Null, fmt.Errorf("%w `%s`", ErrVariableUnbound, name)
}

func (w *World) Set(name string, value Node) {
	p := w.scope
	for p != nil {
		if _, ok := p.globals[name]; ok || p.parent == nil {
			p.globals[name] = value
			return
		}
		p = p.parent
	}
}

func New() *World {
	return &World{
		Stdout: os.Stdout,
		scope: &_Scope{
			globals: map[string]Node{
				"*":             Function(cmdMulti),
				"+":             Function(cmdAdd),
				"-":             Function(cmdSub),
				"/":             Function(cmdDevide),
				"<":             Function(cmdLessThan),
				"<=":            Function(cmdLessOrEqual),
				"=":             Function(cmdEqualOp),
				">":             Function(cmdGreaterThan),
				">=":            Function(cmdGreaterOrEqual),
				"T":             True,
				"append":        Function(cmdAppend),
				"atom":          Function(cmdAtom),
				"block":         Function(cmdBlock),
				"car":           Function(cmdCar),
				"cdr":           Function(cmdCdr),
				"cond":          Function(cmdCond),
				"cons":          Function(cmdCons),
				"defun":         Function(cmdDefun),
				"equal":         Function(cmdEqual),
				"equalp":        Function(cmdEqualOp),
				"if":            Function(cmdIf),
				"lambda":        Function(cmdLambda),
				"let":           Function(cmdLet),
				"list":          Function(cmdList),
				"nil":           Null,
				"parse-integer": Function(cmdParseInt),
				"prin1":         Function(cmdPrin1),
				"princ":         Function(cmdPrinc),
				"print":         Function(cmdPrint),
				"progn":         Function(cmdProgn),
				"quote":         Function(cmdQuote),
				"return":        Function(cmdReturn),
				"return-from":   Function(cmdReturnFrom),
				"setq":          Function(cmdSetq),
				"terpri":        Function(cmdTerpri),
				"truncate":      Function(cmdTruncate),
			},
		},
	}
}

func (w *World) evalListAll(list Node, result []Node) error {
	if err := listToArray(list, result); err != nil {
		return err
	}
	for i := 0; i < len(result); i++ {
		value, err := result[i].Eval(w)
		if err != nil {
			return err
		}
		result[i] = value
	}
	return nil
}

func (w *World) shiftAndEvalCar(list Node) (Node, Node, error) {
	cons, ok := list.(*Cons)
	if !ok {
		return nil, nil, ErrTooFewArguments
	}
	value, err := cons.GetCar().Eval(w)
	if err != nil {
		return nil, nil, err
	}
	return value, cons.Cdr, nil
}

func (w *World) inject(list Node, f func(left, right Node) (Node, error)) (Node, error) {
	result, list, err := w.shiftAndEvalCar(list)
	if err != nil {
		return nil, err
	}
	for HasValue(list) {
		var next Node
		var err error

		next, list, err = w.shiftAndEvalCar(list)
		if err != nil {
			return nil, err
		}
		result, err = f(result, next)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

func (w *World) Interpret(code string) (Node, error) {
	compiled, err := ReadString(code)
	if err != nil {
		return nil, err
	}
	return compiled.Eval(w)
}

func (w *World) InterpretBytes(code []byte) (Node, error) {
	compiled, err := ReadBytes(code)
	if err != nil {
		return nil, err
	}
	return compiled.Eval(w)
}

func (w *World) newWorld(globals map[string]Node, ns *_Scope) *World {
	return &World{
		scope: &_Scope{
			globals: globals,
			parent:  ns,
		},
		Stdout: w.Stdout,
	}
}

func (w *World) Call(f Node, params ...Node) (Node, error) {
	_f, ok := f.(_Callable)
	if !ok {
		return nil, ErrExpectedFunction
	}
	return _f.Call(w, List(params...))
}
