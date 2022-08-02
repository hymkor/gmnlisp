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

func (w *World) DefineVariable(name Symbol, getter func() Node) {
	for w.parent != nil {
		w = w.parent
	}
	if _, ok := w.globals[name]; !ok {
		w.globals[name] = getter()
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
			"/=":                  &FixArgsF{C: 2, F: funNotEqual},
			"1+":                  &FixArgsF{C: 1, F: funOnePlus},
			"1-":                  &FixArgsF{C: 1, F: funOneMinus},
			"<":                   Special(cmdLessThan),
			"<=":                  Special(cmdLessOrEqual),
			"=":                   Special(cmdEqualOp),
			">":                   Special(cmdGreaterThan),
			">=":                  Special(cmdGreaterOrEqual),
			"T":                   True,
			"and":                 Special(cmdAnd),
			"append":              VarArgsF(funAppend),
			"apply":               VarArgsF(funApply),
			"aref":                &FixArgsF{C: 2, F: funAref},
			"assoc":               &FixArgsF{C: 2, F: funAssoc},
			"atom":                &FixArgsF{C: 1, F: funAtom},
			"block":               Special(cmdBlock),
			"cadddr":              &FixArgsF{C: 1, F: funCadddr},
			"caddr":               &FixArgsF{C: 1, F: funCaddr},
			"cadr":                &FixArgsF{C: 1, F: funCadr},
			"car":                 &FixArgsF{C: 1, F: funCar},
			"cdddr":               &FixArgsF{C: 1, F: funCdddr},
			"cddr":                &FixArgsF{C: 1, F: funCddr},
			"cdr":                 &FixArgsF{C: 1, F: funCdr},
			"close":               &FixArgsF{C: 1, F: funClose},
			"coerce":              &FixArgsF{C: 2, F: funCoerce},
			"command":             VarArgsF(funCommand),
			"cond":                Special(cmdCond),
			"cons":                &FixArgsF{C: 2, F: funCons},
			"consp":               &FixArgsF{C: 1, F: funAnyTypep[*Cons]},
			"defmacro":            Special(cmdDefMacro),
			"defparameter":        Special(cmdDefparameter),
			"defun":               Special(cmdDefun),
			"defvar":              Special(cmdDefvar),
			"dolist":              Special(cmdDoList),
			"dotimes":             Special(cmdDoTimes),
			"equal":               Special(cmdEqual),
			"equalp":              Special(cmdEqualOp),
			"evenp":               &FixArgsF{C: 1, F: funEvenp},
			"exit":                Special(cmdQuit),
			"floatp":              &FixArgsF{C: 1, F: funAnyTypep[Float]},
			"foreach":             Special(cmdForeach),
			"funcall":             Special(cmdFunCall),
			"function":            &FixArgsF{C: 1, F: funFunction},
			"if":                  Special(cmdIf),
			"integerp":            &FixArgsF{C: 1, F: funAnyTypep[Integer]},
			"lambda":              Special(cmdLambda),
			"last":                &FixArgsF{C: 1, F: funLast},
			"length":              &FixArgsF{C: 1, F: funLength},
			"let":                 Special(cmdLet),
			"let*":                Special(cmdLetX),
			"list":                VarArgsF(funList),
			"listp":               &FixArgsF{C: 1, F: funListp},
			"load":                &FixArgsF{C: 1, F: funLoad},
			"macroexpand":         Special(cmdMacroExpand),
			"map":                 VarArgsF(funMap),
			"mapcar":              VarArgsF(funMapCar),
			"member":              &FixArgsF{C: 2, F: funMember},
			"minusp":              &FixArgsF{C: 1, F: funMinusp},
			"nil":                 Null,
			"not":                 &FixArgsF{C: 1, F: funNot},
			"nth":                 &FixArgsF{C: 2, F: funNth},
			"nthcdr":              &FixArgsF{C: 2, F: funNthcdr},
			"null":                &FixArgsF{C: 1, F: funNullp},
			"numberp":             &FixArgsF{C: 1, F: funNumberp},
			"oddp":                &FixArgsF{C: 1, F: funOddp},
			"open":                Special(cmdOpen),
			"or":                  Special(cmdOr),
			"parse-integer":       &FixArgsF{C: 1, F: funParseInt},
			"plusp":               &FixArgsF{C: 1, F: funPlusp},
			"prin1":               &FixArgsF{C: 1, F: funPrin1},
			"princ":               &FixArgsF{C: 1, F: funPrinc},
			"print":               &FixArgsF{C: 1, F: funPrint},
			"progn":               Special(cmdProgn),
			"quit":                Special(cmdQuit),
			"quote":               Special(cmdQuote),
			"read":                &FixArgsF{C: 1, F: funRead},
			"read-line":           &FixArgsF{C: 1, F: funReadLine},
			"return":              &FixArgsF{C: 1, F: funReturn},
			"return-from":         Special(cmdReturnFrom),
			"reverse":             &FixArgsF{C: 1, F: funReverse},
			"setq":                Special(cmdSetq),
			"split-string":        &FixArgsF{C: 2, F: funSplitString},
			"strcase":             &FixArgsF{C: 1, F: funStrCase},
			"strcat":              VarArgsF(funStrCat),
			"stringp":             &FixArgsF{C: 1, F: funAnyTypep[String]},
			"strlen":              &FixArgsF{C: 1, F: funStrLen},
			"subst":               &FixArgsF{C: 3, F: funSubst},
			"substr":              Special(cmdSubStr),
			"symbolp":             &FixArgsF{C: 1, F: funAnyTypep[Symbol]},
			"terpri":              Special(cmdTerpri),
			"trace":               Special(cmdTrace),
			"truncate":            &FixArgsF{C: 1, F: funTruncate},
			"typep":               &FixArgsF{C: 2, F: funTypep},
			"while":               Special(cmdWhile),
			"write":               Special(cmdWrite),
			"write-line":          Special(cmdWriteLine),
			"zerop":               &FixArgsF{C: 1, F: funZerop},
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
