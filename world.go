package gmnlisp

import (
	"bufio"
	"bytes"
	"context"
	"fmt"
	"io"
	"math"
	"os"
	"strings"
)

type Scope interface {
	Get(Symbol) (Node, bool)
	Set(Symbol, Node)
}

type Variables map[Symbol]Node

func (m Variables) Get(key Symbol) (Node, bool) {
	value, ok := m[key]
	return value, ok
}

func (m Variables) Set(key Symbol, value Node) {
	m[key] = value
}

type Pair struct {
	Key   Symbol
	Value Node
}

func (m *Pair) Get(key Symbol) (Node, bool) {
	if key == m.Key {
		return m.Value, true
	}
	return Null, false
}

func (m *Pair) Set(key Symbol, value Node) {
	if key == m.Key {
		m.Value = value
	}
	panic("Pair can be set value")
}

type _Shared struct {
	dynamic Variables
	stdout  *_Writer
	errout  *_Writer
	stdin   *_Reader
}

type World struct {
	parent  *World
	lexical Scope
	shared  *_Shared
}

type _Writer struct {
	_Dummy
	io.Writer
}

type _Reader struct {
	_Dummy
	*bufio.Reader
}

func (w *World) Get(name Symbol) (Node, error) {
	for w != nil {
		if value, ok := w.lexical.Get(name); ok {
			return value, nil
		}
		w = w.parent
	}
	return Null, fmt.Errorf("%w `%s`", ErrVariableUnbound, name)
}

func (w *World) SetOrDefineParameter(name Symbol, value Node) {
	for w != nil {
		if _, ok := w.lexical.Get(name); ok || w.parent == nil {
			w.lexical.Set(name, value)
			return
		}
		w = w.parent
	}
}

// DefineGlobal implements (defglobal) of ISLisp or (defparameter) of CommonLisp.
func (w *World) DefineGlobal(name Symbol, value Node) {
	for w.parent != nil {
		w = w.parent
	}
	w.lexical.Set(name, value)
}

func (w *World) DefineVariable(name Symbol, getter func() Node) {
	for w.parent != nil {
		w = w.parent
	}
	if _, ok := w.lexical.Get(name); !ok {
		w.lexical.Set(name, getter())
	}
}

var UseStrict = true

func (w *World) Set(name Symbol, value Node) error {
	if !UseStrict {
		w.SetOrDefineParameter(name, value)
		return nil
	}
	for w != nil {
		if _, ok := w.lexical.Get(name); ok {
			w.lexical.Set(name, value)
			return nil
		}
		w = w.parent
	}
	return ErrVariableUnbound
}

func cmdStandardOutput(ctx context.Context, w *World, list Node) (Node, error) {
	if HasValue(list) {
		return nil, ErrTooManyArguments
	}
	return w.shared.stdout, nil
}

func (w *World) Stdout() io.Writer {
	return w.shared.stdout
}

func (w *World) SetStdout(writer io.Writer) {
	w.shared.stdout = &_Writer{Writer: writer}
}

func cmdErrorOutput(ctx context.Context, w *World, list Node) (Node, error) {
	if HasValue(list) {
		return nil, ErrTooManyArguments
	}
	return w.shared.errout, nil
}

func (w *World) Errout() io.Writer {
	return w.shared.errout
}

func (w *World) SetErrout(writer io.Writer) {
	w.shared.errout = &_Writer{Writer: writer}
}

func cmdStandardInput(ctx context.Context, w *World, list Node) (Node, error) {
	return w.shared.stdin, nil
}
func (w *World) Stdin() *_Reader {
	return w.shared.stdin
}

func New() *World {
	return &World{
		shared: &_Shared{
			dynamic: Variables{},
			stdin:   &_Reader{Reader: bufio.NewReader(os.Stdin)},
			stdout:  &_Writer{Writer: os.Stdout},
			errout:  &_Writer{Writer: os.Stderr},
		},
		lexical: Variables{
			"*":                           SpecialF(cmdMulti),
			"*err-exist*":                 &ErrorNode{Value: os.ErrExist},
			"*err-not-exist*":             &ErrorNode{Value: os.ErrNotExist},
			"*err-quit*":                  &ErrorNode{Value: ErrQuit},
			"*err-too-few-arguments*":     &ErrorNode{Value: ErrTooFewArguments},
			"*err-too-many-arguments*":    &ErrorNode{Value: ErrTooManyArguments},
			"*err-too-short-tokens*":      &ErrorNode{Value: ErrTooShortTokens},
			"*err-variable-unbound*":      &ErrorNode{Value: ErrVariableUnbound},
			"+":                           SpecialF(cmdAdd),
			"-":                           SpecialF(cmdSub),
			"/":                           SpecialF(cmdDevide),
			"/=":                          &Function{C: 2, F: funNotEqual},
			"1+":                          &Function{C: 1, F: funOnePlus},
			"1-":                          &Function{C: 1, F: funOneMinus},
			"<":                           SpecialF(cmdLessThan),
			"<=":                          SpecialF(cmdLessOrEqual),
			"=":                           SpecialF(cmdEqualOp),
			">":                           SpecialF(cmdGreaterThan),
			">=":                          SpecialF(cmdGreaterOrEqual),
			"and":                         SpecialF(cmdAnd),
			"append":                      &Function{C: -1, F: funAppend},
			"apply":                       SpecialF(cmdApply),
			"assoc":                       &Function{C: 2, F: funAssoc},
			"atom":                        &Function{C: 1, F: funAtom},
			"block":                       SpecialF(cmdBlock),
			"cadddr":                      &LeftValueF{C: 1, F: funCadddr},
			"caddr":                       &LeftValueF{C: 1, F: funCaddr},
			"cadr":                        &LeftValueF{C: 1, F: funCadr},
			"car":                         &LeftValueF{C: 1, F: funGetCar},
			"case":                        SpecialF(cmdCase),
			"catch":                       SpecialF(cmdCatch),
			"cdddr":                       &LeftValueF{C: 1, F: funCdddr},
			"cddr":                        &LeftValueF{C: 1, F: funCddr},
			"cdr":                         &LeftValueF{C: 1, F: funGetCdr},
			"close":                       &Function{C: 1, F: funClose},
			"coerce":                      &Function{C: 2, F: funCoerce},
			"command":                     defCommand,
			"concatenate":                 defConcatenate,
			"cond":                        SpecialF(cmdCond),
			"cons":                        &Function{C: 2, F: funCons},
			"consp":                       &Function{C: 1, F: funAnyTypep[*Cons]},
			"create-string-input-stream":  &Function{C: 1, F: funCreateStringInputStream},
			"create-string-output-stream": SpecialF(cmdCreateStringOutputStream),
			"defdynamic":                  SpecialF(cmdDefDynamic),
			"decf":                        SpecialF(cmdDecf),
			"defglobal":                   SpecialF(cmdDefglobal),
			"defmacro":                    SpecialF(cmdDefMacro),
			"defun":                       SpecialF(cmdDefun),
			"dolist":                      SpecialF(cmdDoList),
			"dotimes":                     SpecialF(cmdDoTimes),
			"dynamic":                     SpecialF(cmdDynamic),
			"dynamic-let":                 SpecialF(cmdDynamicLet),
			"elt":                         &LeftValueF{C: 2, F: funElt},
			"eq":                          SpecialF(cmdEq),
			"eql":                         SpecialF(cmdEql),
			"equal":                       SpecialF(cmdEqual),
			"equalp":                      SpecialF(cmdEqualOp),
			"error-output":                SpecialF(cmdErrorOutput),
			"evenp":                       &Function{C: 1, F: funEvenp},
			"exit":                        SpecialF(cmdQuit),
			"find":                        &KWFunction{C: 2, F: funFind},
			"first":                       &LeftValueF{C: 1, F: funGetCar},
			"floatp":                      &Function{C: 1, F: funAnyTypep[Float]},
			"for":                         SpecialF(cmdFor),
			"foreach":                     SpecialF(cmdForeach),
			"format":                      defFormat,
			"format-char":                 &Function{C: 2, F: funFormatChar},
			"format-integer":              &Function{C: 3, F: funFormatInteger},
			"funcall":                     SpecialF(cmdFunCall),
			"function":                    &Function{C: 1, F: funFunction},
			"get-output-stream-string":    &Function{C: 1, F: funGetOutputStreamString},
			"if":                          SpecialF(cmdIf),
			"incf":                        SpecialF(cmdIncf),
			"integerp":                    &Function{C: 1, F: funAnyTypep[Integer]},
			"lambda":                      SpecialF(cmdLambda),
			"last":                        &Function{C: 1, F: funLast},
			"length":                      &Function{C: 1, F: funLength},
			"let":                         SpecialF(cmdLet),
			"let*":                        SpecialF(cmdLetX),
			"list":                        &Function{C: -1, F: funList},
			"listp":                       &Function{C: 1, F: funListp},
			"load":                        &Function{C: 1, F: funLoad},
			"macroexpand":                 SpecialF(cmdMacroExpand),
			"map":                         &Function{C: -1, F: funMap},
			"mapc":                        &Function{C: -1, F: funMapC},
			"mapcar":                      &Function{C: -1, F: funMapCar},
			"mapcan":                      &Function{C: -1, F: funMapCan},
			"mapcon":                      &Function{C: -1, F: funMapCon},
			"mapl":                        &Function{C: -1, F: funMapL},
			"maplist":                     &Function{C: -1, F: funMapList},
			"member":                      &KWFunction{C: 2, F: funMember},
			"minusp":                      &Function{C: 1, F: funMinusp},
			"mod":                         &Function{C: 2, F: funMod},
			"most-negative-fixnum":        Integer(math.MinInt),
			"most-positive-fixnum":        Integer(math.MaxInt),
			"nil":                         Null,
			"not":                         &Function{C: 1, F: funNot},
			"nth":                         &LeftValueF{C: 2, F: funNth},
			"nthcdr":                      &LeftValueF{C: 2, F: funNthcdr},
			"null":                        &Function{C: 1, F: funNullp},
			"numberp":                     &Function{C: 1, F: funNumberp},
			"oddp":                        &Function{C: 1, F: funOddp},
			"open":                        SpecialF(cmdOpen),
			"or":                          SpecialF(cmdOr),
			"parse-integer":               &Function{C: 1, F: funParseInt},
			"pi":                          Float(math.Pi),
			"plusp":                       &Function{C: 1, F: funPlusp},
			"position":                    &KWFunction{C: 2, F: funPosition},
			"prin1":                       &Function{C: 1, F: funPrin1},
			"princ":                       &Function{C: 1, F: funPrinc},
			"print":                       &Function{C: 1, F: funPrint},
			"progn":                       SpecialF(cmdProgn),
			"quit":                        SpecialF(cmdQuit),
			"quote":                       SpecialF(cmdQuote),
			"read":                        defRead,
			"read-from-string":            &Function{C: 1, F: funReadFromString},
			"read-line":                   defReadLine,
			"rem":                         &Function{C: 2, F: funRem},
			"replaca":                     &Function{C: 2, F: funReplaca},
			"replacd":                     &Function{C: 2, F: funReplacd},
			"rest":                        &LeftValueF{C: 1, F: funGetCdr},
			"return":                      &Function{C: 1, F: funReturn},
			"return-from":                 SpecialF(cmdReturnFrom),
			"reverse":                     &Function{C: 1, F: funReverse},
			"second":                      &LeftValueF{C: 1, F: funCadr},
			"setf":                        SpecialF(cmdSetf),
			"setq":                        SpecialF(cmdSetq),
			"standard-input":              SpecialF(cmdStandardInput),
			"standard-output":             SpecialF(cmdStandardOutput),
			"strcase":                     &Function{C: 1, F: funStrCase},
			"string-append":               &Function{C: -1, F: funStringAppend},
			"stringp":                     &Function{C: 1, F: funAnyTypep[String]},
			"subseq":                      &LeftValueF{C: -1, F: funSubSeq},
			"subst":                       &Function{C: 3, F: funSubst},
			"symbolp":                     &Function{C: 1, F: funAnyTypep[Symbol]},
			"t":                           True,
			"terpri":                      &Function{C: -1, F: funTerpri},
			"third":                       &LeftValueF{C: 1, F: funCaddr},
			"throw":                       &Function{C: 2, F: funThrow},
			"to-utf32":                    &Function{C: 1, F: funToUTF32},
			"to-utf8":                     &Function{C: 1, F: funToUTF8},
			"trace":                       SpecialF(cmdTrace),
			"truncate":                    &Function{C: 1, F: funTruncate},
			"typep":                       &Function{C: 2, F: funTypep},
			"unless":                      SpecialF(cmdUnless),
			"unwind-protect":              SpecialF(cmdUnwindProtect),
			"when":                        SpecialF(cmdWhen),
			"while":                       SpecialF(cmdWhile),
			"with-handler":                SpecialF(cmdWithHandler),
			"with-open-file":              SpecialF(cmdWithOpenFile),
			"write":                       &KWFunction{C: 1, F: funWrite},
			"write-line":                  SpecialF(cmdWriteLine),
			"zerop":                       &Function{C: 1, F: funZerop},
		},
	}
}

func (w *World) ShiftAndEvalCar(ctx context.Context, list Node) (Node, Node, error) {
	first, list, err := Shift(list)
	if err != nil {
		return nil, list, err
	}
	value, err := first.Eval(ctx, w)
	if err != nil {
		return nil, list, err
	}
	return value, list, nil
}

func (w *World) inject(ctx context.Context, list Node, f func(left, right Node) (Node, error)) (Node, error) {
	result, list, err := w.ShiftAndEvalCar(ctx, list)
	if err != nil {
		return nil, err
	}
	for HasValue(list) {
		var next Node
		var err error

		next, list, err = w.ShiftAndEvalCar(ctx, list)
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
	compiled, err := ReadAll(strings.NewReader(code))
	if err != nil {
		return nil, err
	}
	return w.InterpretNodes(ctx, compiled)
}

func (w *World) InterpretBytes(ctx context.Context, code []byte) (Node, error) {
	compiled, err := ReadAll(bytes.NewReader(code))
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

func (w *World) Let(scope Scope) *World {
	return &World{
		parent:  w,
		lexical: scope,
		shared:  w.shared,
	}
}

func (w *World) Assert(equation string, expect Node) string {
	result, err := w.Interpret(context.TODO(), equation)
	if err != nil {
		return fmt.Sprintf("%s: %s", equation, err.Error())
	}
	if !result.Equals(expect, EQUAL) {
		return fmt.Sprintf("%s != %s (was %s)", equation, toString(expect, PRINT), toString(result, PRINT))
	}
	return ""
}
