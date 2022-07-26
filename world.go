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

func (w *World) Set(name string, value Node) {
	for w != nil {
		if _, ok := w.globals[name]; ok || w.parent == nil {
			w.globals[name] = value
			return
		}
		w = w.parent
	}
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
	w.Set(standardOutput, Writer{Writer: writer})
}

func New() *World {
	return &World{
		globals: map[string]Node{
			"*":                   Function(cmdMulti),
			"+":                   Function(cmdAdd),
			"-":                   Function(cmdSub),
			"--get-all-symbols--": Function(cmdGetAllSymbols),
			"/":                   Function(cmdDevide),
			"/=":                  &EasyFunc{C: 2, F: cmdNotEqual},
			"<":                   Function(cmdLessThan),
			"<=":                  Function(cmdLessOrEqual),
			"=":                   Function(cmdEqualOp),
			">":                   Function(cmdGreaterThan),
			">=":                  Function(cmdGreaterOrEqual),
			"T":                   True,
			"and":                 Function(cmdAnd),
			"append":              Function(cmdAppend),
			"assoc":               &EasyFunc{C: 2, F: cmdAssoc},
			"atom":                &EasyFunc{C: 1, F: cmdAtom},
			"block":               Function(cmdBlock),
			"cadddr":              &EasyFunc{C: 1, F: cmdCadddr},
			"caddr":               &EasyFunc{C: 1, F: cmdCaddr},
			"cadr":                &EasyFunc{C: 1, F: cmdCadr},
			"car":                 &EasyFunc{C: 1, F: cmdCar},
			"cdddr":               &EasyFunc{C: 1, F: cmdCdddr},
			"cddr":                &EasyFunc{C: 1, F: cmdCddr},
			"cdr":                 &EasyFunc{C: 1, F: cmdCdr},
			"close":               &EasyFunc{C: 1, F: cmdClose},
			"command":             Function(cmdCommand),
			"cond":                Function(cmdCond),
			"cons":                &EasyFunc{C: 2, F: cmdCons},
			"defmacro":            Function(cmdDefMacro),
			"defun":               Function(cmdDefun),
			"equal":               Function(cmdEqual),
			"equalp":              Function(cmdEqualOp),
			"exit":                Function(cmdQuit),
			"foreach":             Function(cmdForeach),
			"funcall":             Function(cmdFunCall),
			"function":            &EasyFunc{C: 1, F: cmdFunction},
			"if":                  Function(cmdIf),
			"lambda":              Function(cmdLambda),
			"length":              &EasyFunc{C: 1, F: cmdLength},
			"let":                 Function(cmdLet),
			"list":                Function(cmdList),
			"listp":               &EasyFunc{C: 1, F: cmdListp},
			"load":                &EasyFunc{C: 1, F: cmdLoad},
			"macroexpand":         Function(cmdMacroExpand),
			"mapcar":              Function(cmdMapCar),
			"member":              &EasyFunc{C: 2, F: cmdMember},
			"nil":                 Null,
			"not":                 &EasyFunc{C: 1, F: cmdNot},
			"open":                Function(cmdOpen),
			"or":                  Function(cmdOr),
			"parse-integer":       &EasyFunc{C: 1, F: cmdParseInt},
			"prin1":               &EasyFunc{C: 1, F: cmdPrin1},
			"princ":               &EasyFunc{C: 1, F: cmdPrinc},
			"print":               &EasyFunc{C: 1, F: cmdPrint},
			"progn":               Function(cmdProgn),
			"quit":                Function(cmdQuit),
			"quote":               Function(cmdQuote),
			"read":                &EasyFunc{C: 1, F: cmdRead},
			"read-line":           &EasyFunc{C: 1, F: cmdReadLine},
			"return":              &EasyFunc{C: 1, F: cmdReturn},
			"return-from":         Function(cmdReturnFrom),
			"reverse":             &EasyFunc{C: 1, F: cmdReverse},
			"setq":                Function(cmdSetq),
			"strcase":             &EasyFunc{C: 1, F: cmdStrCase},
			"strcat":              Function(cmdStrCat),
			"strlen":              &EasyFunc{C: 1, F: cmdStrLen},
			"substr":              Function(cmdSubStr),
			"terpri":              Function(cmdTerpri),
			"trace":               Function(cmdTrace),
			"truncate":            &EasyFunc{C: 1, F: cmdTruncate},
			"while":               Function(cmdWhile),
			"write":               Function(cmdWrite),
			"write-line":          Function(cmdWriteLine),
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
