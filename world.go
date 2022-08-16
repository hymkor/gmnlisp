package gmnlisp

import (
	"bufio"
	"context"
	"fmt"
	"io"
	"math"
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

type _Reader struct {
	_Dummy
	*bufio.Reader
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

const (
	errorOutput    = "*error-output*"
	standardInput  = "*standard-input*"
	standardOutput = "*standard-output*"
)

var (
	stderr = &Writer{Writer: os.Stderr}
	stdin  = &_Reader{Reader: bufio.NewReader(os.Stdin)}
	stdout = &Writer{Writer: os.Stdout}
)

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

func (w *World) Stdin() (*_Reader, error) {
	stdin, err := w.Get(standardInput)
	if err != nil {
		return nil, err
	}
	_stdin, ok := stdin.(*_Reader)
	if !ok {
		return nil, ErrExpectedReader
	}
	return _stdin, nil
}

func New() *World {
	return &World{
		globals: map[Symbol]Node{
			"*":                        SpecialF(cmdMulti),
			"*err-exist*":              &ErrorNode{Value: os.ErrExist},
			"*err-not-exist*":          &ErrorNode{Value: os.ErrNotExist},
			"*err-quit*":               &ErrorNode{Value: ErrQuit},
			"*err-too-few-arguments*":  &ErrorNode{Value: ErrTooFewArguments},
			"*err-too-many-arguments*": &ErrorNode{Value: ErrTooManyArguments},
			"*err-too-short-tokens*":   &ErrorNode{Value: ErrTooShortTokens},
			"+":                        SpecialF(cmdAdd),
			"-":                        SpecialF(cmdSub),
			"--get-all-symbols--":      SpecialF(cmdGetAllSymbols),
			"/":                        SpecialF(cmdDevide),
			"/=":                       &Function{C: 2, F: funNotEqual},
			"1+":                       &Function{C: 1, F: funOnePlus},
			"1-":                       &Function{C: 1, F: funOneMinus},
			"<":                        SpecialF(cmdLessThan),
			"<=":                       SpecialF(cmdLessOrEqual),
			"=":                        SpecialF(cmdEqualOp),
			">":                        SpecialF(cmdGreaterThan),
			">=":                       SpecialF(cmdGreaterOrEqual),
			"and":                      SpecialF(cmdAnd),
			"append":                   &Function{C: -1, F: funAppend},
			"apply":                    SpecialF(cmdApply),
			"aref":                     &LeftValueF{C: 2, F: funAref},
			"assoc":                    &Function{C: 2, F: funAssoc},
			"atom":                     &Function{C: 1, F: funAtom},
			"block":                    SpecialF(cmdBlock),
			"cadddr":                   &LeftValueF{C: 1, F: funCadddr},
			"caddr":                    &LeftValueF{C: 1, F: funCaddr},
			"cadr":                     &LeftValueF{C: 1, F: funCadr},
			"car":                      &LeftValueF{C: 1, F: funGetCar},
			"case":                     SpecialF(cmdCase),
			"cdddr":                    &LeftValueF{C: 1, F: funCdddr},
			"cddr":                     &LeftValueF{C: 1, F: funCddr},
			"cdr":                      &LeftValueF{C: 1, F: funGetCdr},
			"close":                    &Function{C: 1, F: funClose},
			"coerce":                   &Function{C: 2, F: funCoerce},
			"command":                  &Function{C: -1, F: funCommand},
			"concatenate":              &Function{C: -1, F: funConcatenate},
			"cond":                     SpecialF(cmdCond),
			"cons":                     &Function{C: 2, F: funCons},
			"consp":                    &Function{C: 1, F: funAnyTypep[*Cons]},
			"decf":                     SpecialF(cmdDecf),
			"defmacro":                 SpecialF(cmdDefMacro),
			"defparameter":             SpecialF(cmdDefparameter),
			"defun":                    SpecialF(cmdDefun),
			"defvar":                   SpecialF(cmdDefvar),
			"dolist":                   SpecialF(cmdDoList),
			"dotimes":                  SpecialF(cmdDoTimes),
			"eq":                       SpecialF(cmdEq),
			"eql":                      SpecialF(cmdEql),
			"equal":                    SpecialF(cmdEqual),
			"equalp":                   SpecialF(cmdEqualOp),
			"evenp":                    &Function{C: 1, F: funEvenp},
			"exit":                     SpecialF(cmdQuit),
			"find":                     &KWFunction{C: 2, F: funFind},
			"first":                    &LeftValueF{C: 1, F: funGetCar},
			"floatp":                   &Function{C: 1, F: funAnyTypep[Float]},
			"foreach":                  SpecialF(cmdForeach),
			"format":                   &Function{C: -1, F: funFormat},
			"funcall":                  SpecialF(cmdFunCall),
			"function":                 &Function{C: 1, F: funFunction},
			"handler-case":             SpecialF(cmdHandlerCase),
			"if":                       SpecialF(cmdIf),
			"incf":                     SpecialF(cmdIncf),
			"integerp":                 &Function{C: 1, F: funAnyTypep[Integer]},
			"lambda":                   SpecialF(cmdLambda),
			"last":                     &Function{C: 1, F: funLast},
			"length":                   &Function{C: 1, F: funLength},
			"let":                      SpecialF(cmdLet),
			"let*":                     SpecialF(cmdLetX),
			"list":                     &Function{C: -1, F: funList},
			"listp":                    &Function{C: 1, F: funListp},
			"load":                     &Function{C: 1, F: funLoad},
			"loop":                     SpecialF(cmdLoop),
			"macroexpand":              SpecialF(cmdMacroExpand),
			"map":                      &Function{C: -1, F: funMap},
			"mapcar":                   &Function{C: -1, F: funMapCar},
			"member":                   &KWFunction{C: 2, F: funMember},
			"minusp":                   &Function{C: 1, F: funMinusp},
			"mod":                      &Function{C: 2, F: funMod},
			"most-negative-fixnum":     Integer(math.MinInt),
			"most-positive-fixnum":     Integer(math.MaxInt),
			"nil":                      Null,
			"not":                      &Function{C: 1, F: funNot},
			"nth":                      &LeftValueF{C: 2, F: funNth},
			"nthcdr":                   &LeftValueF{C: 2, F: funNthcdr},
			"null":                     &Function{C: 1, F: funNullp},
			"numberp":                  &Function{C: 1, F: funNumberp},
			"oddp":                     &Function{C: 1, F: funOddp},
			"open":                     SpecialF(cmdOpen),
			"or":                       SpecialF(cmdOr),
			"parse-integer":            &Function{C: 1, F: funParseInt},
			"pi":                       Float(math.Pi),
			"plusp":                    &Function{C: 1, F: funPlusp},
			"position":                 &KWFunction{C: 2, F: funPosition},
			"prin1":                    &Function{C: 1, F: funPrin1},
			"princ":                    &Function{C: 1, F: funPrinc},
			"print":                    &Function{C: 1, F: funPrint},
			"progn":                    SpecialF(cmdProgn),
			"quit":                     SpecialF(cmdQuit),
			"quote":                    SpecialF(cmdQuote),
			"read":                     &Function{C: 1, F: funRead},
			"read-line":                &Function{C: -1, F: funReadLine},
			"rem":                      &Function{C: 2, F: funRem},
			"replaca":                  &Function{C: 2, F: funReplaca},
			"replacd":                  &Function{C: 2, F: funReplacd},
			"rest":                     &LeftValueF{C: 1, F: funGetCdr},
			"return":                   &Function{C: 1, F: funReturn},
			"return-from":              SpecialF(cmdReturnFrom),
			"reverse":                  &Function{C: 1, F: funReverse},
			"second":                   &LeftValueF{C: 1, F: funCadr},
			"setf":                     SpecialF(cmdSetf),
			"setq":                     SpecialF(cmdSetq),
			"split-string":             &Function{C: 2, F: funSplitString},
			"strcase":                  &Function{C: 1, F: funStrCase},
			"strcat":                   &Function{C: -1, F: funStrCat},
			"stringp":                  &Function{C: 1, F: funAnyTypep[String]},
			"strlen":                   &Function{C: 1, F: funStrLen},
			"subseq":                   &LeftValueF{C: -1, F: funSubSeq},
			"subst":                    &Function{C: 3, F: funSubst},
			"substr":                   SpecialF(cmdSubStr),
			"symbolp":                  &Function{C: 1, F: funAnyTypep[Symbol]},
			"t":                        True,
			"terpri":                   &Function{C: -1, F: funTerpri},
			"third":                    &LeftValueF{C: 1, F: funCaddr},
			"trace":                    SpecialF(cmdTrace),
			"truncate":                 &Function{C: 1, F: funTruncate},
			"typep":                    &Function{C: 2, F: funTypep},
			"unless":                   SpecialF(cmdUnless),
			"when":                     SpecialF(cmdWhen),
			"while":                    SpecialF(cmdWhile),
			"with-open-file":           SpecialF(cmdWithOpenFile),
			"write":                    &KWFunction{C: 1, F: funWrite},
			"write-line":               SpecialF(cmdWriteLine),
			"zerop":                    &Function{C: 1, F: funZerop},
			errorOutput:                stderr,
			standardInput:              stdin,
			standardOutput:             stdout,
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
