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
			"/=":                  &Function{C: 2, F: cmdNotEqual},
			"1+":                  &Function{C: 1, F: cmdOnePlus},
			"1-":                  &Function{C: 1, F: cmdOneMinus},
			"<":                   Special(cmdLessThan),
			"<=":                  Special(cmdLessOrEqual),
			"=":                   Special(cmdEqualOp),
			">":                   Special(cmdGreaterThan),
			">=":                  Special(cmdGreaterOrEqual),
			"T":                   True,
			"and":                 Special(cmdAnd),
			"append":              Special(cmdAppend),
			"assoc":               &Function{C: 2, F: cmdAssoc},
			"atom":                &Function{C: 1, F: cmdAtom},
			"block":               Special(cmdBlock),
			"cadddr":              &Function{C: 1, F: cmdCadddr},
			"caddr":               &Function{C: 1, F: cmdCaddr},
			"cadr":                &Function{C: 1, F: cmdCadr},
			"car":                 &Function{C: 1, F: cmdCar},
			"cdddr":               &Function{C: 1, F: cmdCdddr},
			"cddr":                &Function{C: 1, F: cmdCddr},
			"cdr":                 &Function{C: 1, F: cmdCdr},
			"close":               &Function{C: 1, F: cmdClose},
			"command":             FunctionN(funCommand),
			"cond":                Special(cmdCond),
			"cons":                &Function{C: 2, F: cmdCons},
			"defmacro":            Special(cmdDefMacro),
			"defparameter":        Special(cmdDefparameter),
			"defun":               Special(cmdDefun),
			"defvar":              Special(cmdDefvar),
			"equal":               Special(cmdEqual),
			"equalp":              Special(cmdEqualOp),
			"exit":                Special(cmdQuit),
			"foreach":             Special(cmdForeach),
			"funcall":             Special(cmdFunCall),
			"function":            &Function{C: 1, F: cmdFunction},
			"if":                  Special(cmdIf),
			"lambda":              Special(cmdLambda),
			"length":              &Function{C: 1, F: cmdLength},
			"let":                 Special(cmdLet),
			"let*":                Special(cmdLetX),
			"list":                Special(cmdList),
			"listp":               &Function{C: 1, F: cmdListp},
			"load":                &Function{C: 1, F: cmdLoad},
			"macroexpand":         Special(cmdMacroExpand),
			"mapcar":              Special(cmdMapCar),
			"member":              &Function{C: 2, F: cmdMember},
			"nil":                 Null,
			"not":                 &Function{C: 1, F: cmdNot},
			"nth":                 &Function{C: 2, F: cmdNth},
			"nthcdr":              &Function{C: 2, F: cmdNthcdr},
			"open":                Special(cmdOpen),
			"or":                  Special(cmdOr),
			"parse-integer":       &Function{C: 1, F: cmdParseInt},
			"prin1":               &Function{C: 1, F: cmdPrin1},
			"princ":               &Function{C: 1, F: cmdPrinc},
			"print":               &Function{C: 1, F: cmdPrint},
			"progn":               Special(cmdProgn),
			"quit":                Special(cmdQuit),
			"quote":               Special(cmdQuote),
			"read":                &Function{C: 1, F: cmdRead},
			"read-line":           &Function{C: 1, F: cmdReadLine},
			"return":              &Function{C: 1, F: cmdReturn},
			"return-from":         Special(cmdReturnFrom),
			"reverse":             &Function{C: 1, F: cmdReverse},
			"setq":                Special(cmdSetq),
			"split-string":        &Function{C: 2, F: cmdSplitString},
			"strcase":             &Function{C: 1, F: cmdStrCase},
			"strcat":              FunctionN(funStrCat),
			"strlen":              &Function{C: 1, F: cmdStrLen},
			"subst":               &Function{C: 3, F: cmdSubst},
			"substr":              Special(cmdSubStr),
			"terpri":              Special(cmdTerpri),
			"trace":               Special(cmdTrace),
			"truncate":            &Function{C: 1, F: cmdTruncate},
			"while":               Special(cmdWhile),
			"write":               Special(cmdWrite),
			"write-line":          Special(cmdWriteLine),
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
