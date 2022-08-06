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
			"*":                   SpecialF(cmdMulti),
			"+":                   SpecialF(cmdAdd),
			"-":                   SpecialF(cmdSub),
			"--get-all-symbols--": SpecialF(cmdGetAllSymbols),
			"/":                   SpecialF(cmdDevide),
			"/=":                  &FixArgsF{C: 2, F: funNotEqual},
			"1+":                  &FixArgsF{C: 1, F: funOnePlus},
			"1-":                  &FixArgsF{C: 1, F: funOneMinus},
			"<":                   SpecialF(cmdLessThan),
			"<=":                  SpecialF(cmdLessOrEqual),
			"=":                   SpecialF(cmdEqualOp),
			">":                   SpecialF(cmdGreaterThan),
			">=":                  SpecialF(cmdGreaterOrEqual),
			"T":                   True,
			"and":                 SpecialF(cmdAnd),
			"append":              VarArgsF(funAppend),
			"apply":               SpecialF(cmdApply),
			"aref":                &LeftValue2F{C: 2, F: funAref},
			"assoc":               &FixArgsF{C: 2, F: funAssoc},
			"atom":                &FixArgsF{C: 1, F: funAtom},
			"block":               SpecialF(cmdBlock),
			"cadddr":              &LeftValueF{C: 1, F: funCadddr},
			"caddr":               &LeftValueF{C: 1, F: funCaddr},
			"cadr":                &LeftValueF{C: 1, F: funCadr},
			"car":                 &LeftValue2F{C: 1, F: funGetCar},
			"cdddr":               &LeftValueF{C: 1, F: funCdddr},
			"cddr":                &LeftValueF{C: 1, F: funCddr},
			"cdr":                 &LeftValue2F{C: 1, F: funGetCdr},
			"close":               &FixArgsF{C: 1, F: funClose},
			"coerce":              &FixArgsF{C: 2, F: funCoerce},
			"command":             VarArgsF(funCommand),
			"concatenate":         VarArgsF(funConcatenate),
			"cond":                SpecialF(cmdCond),
			"cons":                &FixArgsF{C: 2, F: funCons},
			"consp":               &FixArgsF{C: 1, F: funAnyTypep[*Cons]},
			"defmacro":            SpecialF(cmdDefMacro),
			"defparameter":        SpecialF(cmdDefparameter),
			"defun":               SpecialF(cmdDefun),
			"defvar":              SpecialF(cmdDefvar),
			"dolist":              SpecialF(cmdDoList),
			"dotimes":             SpecialF(cmdDoTimes),
			"equal":               SpecialF(cmdEqual),
			"equalp":              SpecialF(cmdEqualOp),
			"evenp":               &FixArgsF{C: 1, F: funEvenp},
			"exit":                SpecialF(cmdQuit),
			"floatp":              &FixArgsF{C: 1, F: funAnyTypep[Float]},
			"foreach":             SpecialF(cmdForeach),
			"funcall":             SpecialF(cmdFunCall),
			"function":            &FixArgsF{C: 1, F: funFunction},
			"if":                  SpecialF(cmdIf),
			"integerp":            &FixArgsF{C: 1, F: funAnyTypep[Integer]},
			"lambda":              SpecialF(cmdLambda),
			"last":                &FixArgsF{C: 1, F: funLast},
			"length":              &FixArgsF{C: 1, F: funLength},
			"let":                 SpecialF(cmdLet),
			"let*":                SpecialF(cmdLetX),
			"list":                VarArgsF(funList),
			"listp":               &FixArgsF{C: 1, F: funListp},
			"load":                &FixArgsF{C: 1, F: funLoad},
			"macroexpand":         SpecialF(cmdMacroExpand),
			"map":                 VarArgsF(funMap),
			"mapcar":              VarArgsF(funMapCar),
			"member":              &FixArgsF{C: 2, F: funMember},
			"minusp":              &FixArgsF{C: 1, F: funMinusp},
			"nil":                 Null,
			"not":                 &FixArgsF{C: 1, F: funNot},
			"nth":                 &LeftValueF{C: 2, F: funNth},
			"nthcdr":              &LeftValueF{C: 2, F: funNthcdr},
			"null":                &FixArgsF{C: 1, F: funNullp},
			"numberp":             &FixArgsF{C: 1, F: funNumberp},
			"oddp":                &FixArgsF{C: 1, F: funOddp},
			"open":                SpecialF(cmdOpen),
			"or":                  SpecialF(cmdOr),
			"parse-integer":       &FixArgsF{C: 1, F: funParseInt},
			"plusp":               &FixArgsF{C: 1, F: funPlusp},
			"position":            &FixArgsF{C: 2, F: funPosition},
			"prin1":               &FixArgsF{C: 1, F: funPrin1},
			"princ":               &FixArgsF{C: 1, F: funPrinc},
			"print":               &FixArgsF{C: 1, F: funPrint},
			"progn":               SpecialF(cmdProgn),
			"quit":                SpecialF(cmdQuit),
			"quote":               SpecialF(cmdQuote),
			"read":                &FixArgsF{C: 1, F: funRead},
			"read-line":           &FixArgsF{C: 1, F: funReadLine},
			"return":              &FixArgsF{C: 1, F: funReturn},
			"return-from":         SpecialF(cmdReturnFrom),
			"reverse":             &FixArgsF{C: 1, F: funReverse},
			"setf":                SpecialF(cmdSetf),
			"setq":                SpecialF(cmdSetq),
			"split-string":        &FixArgsF{C: 2, F: funSplitString},
			"strcase":             &FixArgsF{C: 1, F: funStrCase},
			"strcat":              VarArgsF(funStrCat),
			"stringp":             &FixArgsF{C: 1, F: funAnyTypep[String]},
			"strlen":              &FixArgsF{C: 1, F: funStrLen},
			"subseq":              VarArgsF(funSubSeq),
			"subst":               &FixArgsF{C: 3, F: funSubst},
			"substr":              SpecialF(cmdSubStr),
			"symbolp":             &FixArgsF{C: 1, F: funAnyTypep[Symbol]},
			"terpri":              SpecialF(cmdTerpri),
			"trace":               SpecialF(cmdTrace),
			"truncate":            &FixArgsF{C: 1, F: funTruncate},
			"typep":               &FixArgsF{C: 2, F: funTypep},
			"while":               SpecialF(cmdWhile),
			"write":               SpecialF(cmdWrite),
			"write-line":          SpecialF(cmdWriteLine),
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
