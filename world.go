package gmnlisp

import (
	"context"
	"fmt"
	"io"
	"os"
)

type World struct {
	parent  *World
	globals map[string]Node
}

type Writer struct {
	_Dummy
	io.Writer
}

func (w *World) each(f func(string, Node) bool) {
	for w != nil {
		for name, value := range w.globals {
			if !f(name, value) {
				return
			}
		}
		w = w.parent
	}
}

func (w *World) Get(name string) (Node, error) {
	for w != nil {
		if value, ok := w.globals[name]; ok {
			return value, nil
		}
		w = w.parent
	}
	return Null, fmt.Errorf("%w `%s`", ErrVariableUnbound, name)
}

func (w *World) SetOrNew(name string, value Node) {
	for w != nil {
		if _, ok := w.globals[name]; ok || w.parent == nil {
			w.globals[name] = value
			return
		}
		w = w.parent
	}
}

var UseStrict = true

func (w *World) Set(name string, value Node) error {
	if !UseStrict {
		w.SetOrNew(name, value)
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
	w.SetOrNew(standardOutput, Writer{Writer: writer})
}

func New() *World {
	return &World{
		globals: map[string]Node{
			"*":                   SpecialFunc(cmdMulti),
			"+":                   SpecialFunc(cmdAdd),
			"-":                   SpecialFunc(cmdSub),
			"--get-all-symbols--": SpecialFunc(cmdGetAllSymbols),
			"/":                   SpecialFunc(cmdDevide),
			"/=":                  &EasyFunc{C: 2, F: cmdNotEqual},
			"1+":                  &EasyFunc{C: 1, F: cmdOnePlus},
			"1-":                  &EasyFunc{C: 1, F: cmdOneMinus},
			"<":                   SpecialFunc(cmdLessThan),
			"<=":                  SpecialFunc(cmdLessOrEqual),
			"=":                   SpecialFunc(cmdEqualOp),
			">":                   SpecialFunc(cmdGreaterThan),
			">=":                  SpecialFunc(cmdGreaterOrEqual),
			"T":                   True,
			"and":                 SpecialFunc(cmdAnd),
			"append":              SpecialFunc(cmdAppend),
			"assoc":               &EasyFunc{C: 2, F: cmdAssoc},
			"atom":                &EasyFunc{C: 1, F: cmdAtom},
			"block":               SpecialFunc(cmdBlock),
			"cadddr":              &EasyFunc{C: 1, F: cmdCadddr},
			"caddr":               &EasyFunc{C: 1, F: cmdCaddr},
			"cadr":                &EasyFunc{C: 1, F: cmdCadr},
			"car":                 &EasyFunc{C: 1, F: cmdCar},
			"cdddr":               &EasyFunc{C: 1, F: cmdCdddr},
			"cddr":                &EasyFunc{C: 1, F: cmdCddr},
			"cdr":                 &EasyFunc{C: 1, F: cmdCdr},
			"close":               &EasyFunc{C: 1, F: cmdClose},
			"command":             SpecialFunc(cmdCommand),
			"cond":                SpecialFunc(cmdCond),
			"cons":                &EasyFunc{C: 2, F: cmdCons},
			"defmacro":            SpecialFunc(cmdDefMacro),
			"defun":               SpecialFunc(cmdDefun),
			"defvar":              SpecialFunc(cmdDefvar),
			"equal":               SpecialFunc(cmdEqual),
			"equalp":              SpecialFunc(cmdEqualOp),
			"exit":                SpecialFunc(cmdQuit),
			"foreach":             SpecialFunc(cmdForeach),
			"funcall":             SpecialFunc(cmdFunCall),
			"function":            &EasyFunc{C: 1, F: cmdFunction},
			"if":                  SpecialFunc(cmdIf),
			"lambda":              SpecialFunc(cmdLambda),
			"length":              &EasyFunc{C: 1, F: cmdLength},
			"let":                 SpecialFunc(cmdLet),
			"list":                SpecialFunc(cmdList),
			"listp":               &EasyFunc{C: 1, F: cmdListp},
			"load":                &EasyFunc{C: 1, F: cmdLoad},
			"macroexpand":         SpecialFunc(cmdMacroExpand),
			"mapcar":              SpecialFunc(cmdMapCar),
			"member":              &EasyFunc{C: 2, F: cmdMember},
			"nil":                 Null,
			"not":                 &EasyFunc{C: 1, F: cmdNot},
			"nth":                 &EasyFunc{C: 2, F: cmdNth},
			"nthcdr":              &EasyFunc{C: 2, F: cmdNthcdr},
			"open":                SpecialFunc(cmdOpen),
			"or":                  SpecialFunc(cmdOr),
			"parse-integer":       &EasyFunc{C: 1, F: cmdParseInt},
			"prin1":               &EasyFunc{C: 1, F: cmdPrin1},
			"princ":               &EasyFunc{C: 1, F: cmdPrinc},
			"print":               &EasyFunc{C: 1, F: cmdPrint},
			"progn":               SpecialFunc(cmdProgn),
			"quit":                SpecialFunc(cmdQuit),
			"quote":               SpecialFunc(cmdQuote),
			"read":                &EasyFunc{C: 1, F: cmdRead},
			"read-line":           &EasyFunc{C: 1, F: cmdReadLine},
			"return":              &EasyFunc{C: 1, F: cmdReturn},
			"return-from":         SpecialFunc(cmdReturnFrom),
			"reverse":             &EasyFunc{C: 1, F: cmdReverse},
			"setq":                SpecialFunc(cmdSetq),
			"split-string":        &EasyFunc{C: 2, F: cmdSplitString},
			"strcase":             &EasyFunc{C: 1, F: cmdStrCase},
			"strcat":              SpecialFunc(cmdStrCat),
			"strlen":              &EasyFunc{C: 1, F: cmdStrLen},
			"subst":               &EasyFunc{C: 3, F: cmdSubst},
			"substr":              SpecialFunc(cmdSubStr),
			"terpri":              SpecialFunc(cmdTerpri),
			"trace":               SpecialFunc(cmdTrace),
			"truncate":            &EasyFunc{C: 1, F: cmdTruncate},
			"while":               SpecialFunc(cmdWhile),
			"write":               SpecialFunc(cmdWrite),
			"write-line":          SpecialFunc(cmdWriteLine),
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
