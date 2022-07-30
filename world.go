package gmnlisp

import (
	"context"
	"fmt"
	"io"
	"os"
)

type World struct {
	parent  *World
	globals map[Symbol]Node
}

type Writer struct {
	_Dummy
	io.Writer
}

func (w *World) each(f func(Symbol, Node) bool) {
	for w != nil {
		for name, value := range w.globals {
			if !f(name, value) {
				return
			}
		}
		w = w.parent
	}
}

func (w *World) Get(name Symbol) (Node, error) {
	for w != nil {
		if value, ok := w.globals[name]; ok {
			return value, nil
		}
		w = w.parent
	}
	return Null, fmt.Errorf("%w `%s`", ErrVariableUnbound, name)
}

func (w *World) SetOrDefineParameter(name Symbol, value Node) {
	for w != nil {
		if _, ok := w.globals[name]; ok || w.parent == nil {
			w.globals[name] = value
			return
		}
		w = w.parent
	}
}

func (w *World) DefineParameter(name Symbol, value Node) {
	for w.parent != nil {
		w = w.parent
	}
	w.globals[name] = value
}

func (w *World) DefineVariable(name Symbol, value Node) {
	for w.parent != nil {
		w = w.parent
	}
	if _, ok := w.globals[name]; !ok {
		w.globals[name] = value
	}
}

var UseStrict = true

func (w *World) Set(name Symbol, value Node) error {
	if !UseStrict {
		w.SetOrDefineParameter(name, value)
		return nil
	}
	for w != nil {
		if _, ok := w.globals[name]; ok {
			w.globals[name] = value
			return nil
		}
		w = w.parent
	}
	return ErrVariableUnbound
}

const standardOutput = "*standard-output*"

func (w *World) Stdout() (io.Writer, error) {
	stdout, err := w.Get(standardOutput)
	if err != nil {
		return nil, err
	}
	_stdout, ok := stdout.(io.Writer)
	if !ok {
		return nil, ErrExpectedWriter
	}
	return _stdout, nil
}

func (w *World) SetStdout(writer io.Writer) {
	w.DefineParameter(standardOutput, Writer{Writer: writer})
}

func New() *World {
	return &World{
		globals: map[Symbol]Node{
			"*":                   Special(cmdMulti),
			"+":                   Special(cmdAdd),
			"-":                   Special(cmdSub),
			"--get-all-symbols--": Special(cmdGetAllSymbols),
			"/":                   Special(cmdDevide),
			"/=":                  &Function{C: 2, F: funNotEqual},
			"1+":                  &Function{C: 1, F: funOnePlus},
			"1-":                  &Function{C: 1, F: funOneMinus},
			"<":                   Special(cmdLessThan),
			"<=":                  Special(cmdLessOrEqual),
			"=":                   Special(cmdEqualOp),
			">":                   Special(cmdGreaterThan),
			">=":                  Special(cmdGreaterOrEqual),
			"T":                   True,
			"and":                 Special(cmdAnd),
			"append":              FunctionN(funAppend),
			"apply":               FunctionN(funApply),
			"assoc":               &Function{C: 2, F: funAssoc},
			"atom":                &Function{C: 1, F: funAtom},
			"block":               Special(cmdBlock),
			"cadddr":              &Function{C: 1, F: funCadddr},
			"caddr":               &Function{C: 1, F: funCaddr},
			"cadr":                &Function{C: 1, F: funCadr},
			"car":                 &Function{C: 1, F: funCar},
			"cdddr":               &Function{C: 1, F: funCdddr},
			"cddr":                &Function{C: 1, F: funCddr},
			"cdr":                 &Function{C: 1, F: funCdr},
			"close":               &Function{C: 1, F: funClose},
			"command":             FunctionN(funCommand),
			"cond":                Special(cmdCond),
			"cons":                &Function{C: 2, F: funCons},
			"defmacro":            Special(cmdDefMacro),
			"defparameter":        Special(cmdDefparameter),
			"defun":               Special(cmdDefun),
			"defvar":              Special(cmdDefvar),
			"dolist":              Special(cmdDoList),
			"dotimes":             Special(cmdDoTimes),
			"equal":               Special(cmdEqual),
			"equalp":              Special(cmdEqualOp),
			"exit":                Special(cmdQuit),
			"foreach":             Special(cmdForeach),
			"funcall":             Special(cmdFunCall),
			"function":            &Function{C: 1, F: funFunction},
			"if":                  Special(cmdIf),
			"integerp":            &Function{C: 1, F: funAnyTypep[Integer]},
			"lambda":              Special(cmdLambda),
			"last":                &Function{C: 1, F: funLast},
			"length":              &Function{C: 1, F: funLength},
			"let":                 Special(cmdLet),
			"let*":                Special(cmdLetX),
			"list":                FunctionN(funList),
			"listp":               &Function{C: 1, F: funListp},
			"load":                &Function{C: 1, F: funLoad},
			"macroexpand":         Special(cmdMacroExpand),
			"mapcar":              FunctionN(funMapCar),
			"member":              &Function{C: 2, F: funMember},
			"minusp":              &Function{C: 1, F: funMinusp},
			"nil":                 Null,
			"not":                 &Function{C: 1, F: funNot},
			"nth":                 &Function{C: 2, F: funNth},
			"nthcdr":              &Function{C: 2, F: funNthcdr},
			"null":                &Function{C: 1, F: funNullp},
			"numberp":             &Function{C: 1, F: funNumberp},
			"open":                Special(cmdOpen),
			"or":                  Special(cmdOr),
			"parse-integer":       &Function{C: 1, F: funParseInt},
			"plusp":               &Function{C: 1, F: funPlusp},
			"prin1":               &Function{C: 1, F: funPrin1},
			"princ":               &Function{C: 1, F: funPrinc},
			"print":               &Function{C: 1, F: funPrint},
			"progn":               Special(cmdProgn),
			"quit":                Special(cmdQuit),
			"quote":               Special(cmdQuote),
			"read":                &Function{C: 1, F: funRead},
			"read-line":           &Function{C: 1, F: funReadLine},
			"return":              &Function{C: 1, F: funReturn},
			"return-from":         Special(cmdReturnFrom),
			"reverse":             &Function{C: 1, F: funReverse},
			"setq":                Special(cmdSetq),
			"split-string":        &Function{C: 2, F: funSplitString},
			"strcase":             &Function{C: 1, F: funStrCase},
			"strcat":              FunctionN(funStrCat),
			"strlen":              &Function{C: 1, F: funStrLen},
			"subst":               &Function{C: 3, F: funSubst},
			"substr":              Special(cmdSubStr),
			"terpri":              Special(cmdTerpri),
			"trace":               Special(cmdTrace),
			"truncate":            &Function{C: 1, F: funTruncate},
			"while":               Special(cmdWhile),
			"write":               Special(cmdWrite),
			"write-line":          Special(cmdWriteLine),
			"zerop":               &Function{C: 1, F: funZerop},
			standardOutput:        &Writer{Writer: os.Stdout},
		},
	}
}

func (w *World) shiftAndEvalCar(ctx context.Context, list Node) (Node, Node, error) {
	first, list, err := shift(list)
	if err != nil {
		return nil, nil, err
	}
	value, err := first.Eval(ctx, w)
	if err != nil {
		return nil, nil, err
	}
	return value, list, nil
}

func (w *World) inject(ctx context.Context, list Node, f func(left, right Node) (Node, error)) (Node, error) {
	result, list, err := w.shiftAndEvalCar(ctx, list)
	if err != nil {
		return nil, err
	}
	for HasValue(list) {
		var next Node
		var err error

		next, list, err = w.shiftAndEvalCar(ctx, list)
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

func (w *World) InterpretNodes(ctx context.Context, ns []Node) (Node, error) {
	var result Node = Null
	var err error

	for _, c := range ns {
		result, err = c.Eval(ctx, w)
		if err != nil {
			return result, err
		}
	}
	return result, nil
}

func (w *World) Interpret(ctx context.Context, code string) (Node, error) {
	compiled, err := ReadString(code)
	if err != nil {
		return nil, err
	}
	return w.InterpretNodes(ctx, compiled)
}

func (w *World) InterpretBytes(ctx context.Context, code []byte) (Node, error) {
	compiled, err := ReadBytes(code)
	if err != nil {
		return nil, err
	}
	return w.InterpretNodes(ctx, compiled)
}

func (w *World) Call(ctx context.Context, f Node, params ...Node) (Node, error) {
	_f, ok := f.(_Callable)
	if !ok {
		return nil, ErrExpectedFunction
	}
	return _f.Call(ctx, w, List(params...))
}
