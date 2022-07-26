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
			"/=":                  Easy(2, cmdNotEqual),
			"<":                   Function(cmdLessThan),
			"<=":                  Function(cmdLessOrEqual),
			"=":                   Function(cmdEqualOp),
			">":                   Function(cmdGreaterThan),
			">=":                  Function(cmdGreaterOrEqual),
			"T":                   True,
			"and":                 Function(cmdAnd),
			"append":              Function(cmdAppend),
			"assoc":               Easy(2, cmdAssoc),
			"atom":                Easy(1, cmdAtom),
			"block":               Function(cmdBlock),
			"cadddr":              Easy(1, cmdCadddr),
			"caddr":               Easy(1, cmdCaddr),
			"cadr":                Easy(1, cmdCadr),
			"car":                 Easy(1, cmdCar),
			"cdddr":               Easy(1, cmdCdddr),
			"cddr":                Easy(1, cmdCddr),
			"cdr":                 Easy(1, cmdCdr),
			"close":               Easy(1, cmdClose),
			"command":             Function(cmdCommand),
			"cond":                Function(cmdCond),
			"cons":                Easy(2, cmdCons),
			"defmacro":            Function(cmdDefMacro),
			"defun":               Function(cmdDefun),
			"equal":               Function(cmdEqual),
			"equalp":              Function(cmdEqualOp),
			"exit":                Function(cmdQuit),
			"foreach":             Function(cmdForeach),
			"funcall":             Function(cmdFunCall),
			"function":            Easy(1, cmdFunction),
			"if":                  Function(cmdIf),
			"lambda":              Function(cmdLambda),
			"length":              Easy(1, cmdLength),
			"let":                 Function(cmdLet),
			"list":                Function(cmdList),
			"listp":               Easy(1, cmdListp),
			"load":                Easy(1, cmdLoad),
			"macroexpand":         Function(cmdMacroExpand),
			"mapcar":              Function(cmdMapCar),
			"member":              Easy(2, cmdMember),
			"nil":                 Null,
			"not":                 Easy(1, cmdNot),
			"open":                Function(cmdOpen),
			"or":                  Function(cmdOr),
			"parse-integer":       Easy(1, cmdParseInt),
			"prin1":               Easy(1, cmdPrin1),
			"princ":               Easy(1, cmdPrinc),
			"print":               Easy(1, cmdPrint),
			"progn":               Function(cmdProgn),
			"quit":                Function(cmdQuit),
			"quote":               Function(cmdQuote),
			"read":                Easy(1, cmdRead),
			"read-line":           Easy(1, cmdReadLine),
			"return":              Easy(1, cmdReturn),
			"return-from":         Function(cmdReturnFrom),
			"reverse":             Easy(1, cmdReverse),
			"setq":                Function(cmdSetq),
			"strcase":             Easy(1, cmdStrCase),
			"strcat":              Function(cmdStrCat),
			"strlen":              Easy(1, cmdStrLen),
			"substr":              Function(cmdSubStr),
			"terpri":              Function(cmdTerpri),
			"trace":               Function(cmdTrace),
			"truncate":            Easy(1, cmdTruncate),
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
