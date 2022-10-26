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
	stdout  *_WriterNode
	errout  *_WriterNode
	stdin   *_ReaderNode
}

type World struct {
	parent  *World
	lexical Scope
	shared  *_Shared
}

type _Reader interface {
	io.RuneScanner
	io.ByteReader
}

type _ReaderNode struct {
	_Dummy
	_Reader
}

type _Writer = io.Writer

type _WriterNode struct {
	_Dummy
	_Writer
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
	w.shared.stdout = &_WriterNode{_Writer: writer}
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
	w.shared.errout = &_WriterNode{_Writer: writer}
}

func cmdStandardInput(ctx context.Context, w *World, list Node) (Node, error) {
	return w.shared.stdin, nil
}
func (w *World) Stdin() *_ReaderNode {
	return w.shared.stdin
}

func New() *World {
	return &World{
		shared: &_Shared{
			dynamic: Variables{},
			stdin:   &_ReaderNode{_Reader: bufio.NewReader(os.Stdin)},
			stdout:  &_WriterNode{_Writer: os.Stdout},
			errout:  &_WriterNode{_Writer: os.Stderr},
		},
		lexical: Variables{
			// *sort*start*
			NewSymbol("*"):                           SpecialF(cmdMulti),
			NewSymbol("*err-exist*"):                 &ErrorNode{Value: os.ErrExist},
			NewSymbol("*err-not-exist*"):             &ErrorNode{Value: os.ErrNotExist},
			NewSymbol("*err-quit*"):                  &ErrorNode{Value: ErrQuit},
			NewSymbol("*err-too-few-arguments*"):     &ErrorNode{Value: ErrTooFewArguments},
			NewSymbol("*err-too-many-arguments*"):    &ErrorNode{Value: ErrTooManyArguments},
			NewSymbol("*err-too-short-tokens*"):      &ErrorNode{Value: ErrTooShortTokens},
			NewSymbol("*err-variable-unbound*"):      &ErrorNode{Value: ErrVariableUnbound},
			NewSymbol("+"):                           SpecialF(cmdAdd),
			NewSymbol("-"):                           SpecialF(cmdSub),
			NewSymbol("/"):                           SpecialF(cmdDevide),
			NewSymbol("/="):                          &Function{C: 2, F: funNotEqual},
			NewSymbol("1+"):                          &Function{C: 1, F: funOnePlus},
			NewSymbol("1-"):                          &Function{C: 1, F: funOneMinus},
			NewSymbol("<"):                           SpecialF(cmdLessThan),
			NewSymbol("<="):                          SpecialF(cmdLessOrEqual),
			NewSymbol("="):                           SpecialF(cmdEqualOp),
			NewSymbol(">"):                           SpecialF(cmdGreaterThan),
			NewSymbol(">="):                          SpecialF(cmdGreaterOrEqual),
			NewSymbol("and"):                         SpecialF(cmdAnd),
			NewSymbol("append"):                      &Function{C: -1, F: funAppend},
			NewSymbol("apply"):                       SpecialF(cmdApply),
			NewSymbol("assoc"):                       &Function{C: 2, F: funAssoc},
			NewSymbol("atom"):                        &Function{C: 1, F: funAtom},
			NewSymbol("block"):                       SpecialF(cmdBlock),
			NewSymbol("car"):                         &LeftValueF{C: 1, F: funGetCar},
			NewSymbol("case"):                        SpecialF(cmdCase),
			NewSymbol("catch"):                       SpecialF(cmdCatch),
			NewSymbol("cdr"):                         &LeftValueF{C: 1, F: funGetCdr},
			NewSymbol("ceiling"):                     &Function{C: 1, F: funCeiling},
			NewSymbol("close"):                       &Function{C: 1, F: funClose},
			NewSymbol("cond"):                        SpecialF(cmdCond),
			NewSymbol("cons"):                        &Function{C: 2, F: funCons},
			NewSymbol("consp"):                       &Function{C: 1, F: funAnyTypep[*Cons]},
			NewSymbol("convert"):                     SpecialF(cmdConvert),
			NewSymbol("create-string"):               &Function{C: -1, F: funCreateString},
			NewSymbol("create-string-input-stream"):  &Function{C: 1, F: funCreateStringInputStream},
			NewSymbol("create-string-output-stream"): SpecialF(cmdCreateStringOutputStream),
			NewSymbol("decf"):                        SpecialF(cmdDecf),
			NewSymbol("defdynamic"):                  SpecialF(cmdDefDynamic),
			NewSymbol("defglobal"):                   SpecialF(cmdDefglobal),
			NewSymbol("defmacro"):                    SpecialF(cmdDefMacro),
			NewSymbol("defun"):                       SpecialF(cmdDefun),
			NewSymbol("dynamic"):                     LeftValueCmd(cmdDynamic),
			NewSymbol("dynamic-let"):                 SpecialF(cmdDynamicLet),
			NewSymbol("elt"):                         &LeftValueF{C: 2, F: funElt},
			NewSymbol("eq"):                          SpecialF(cmdEq),
			NewSymbol("eql"):                         SpecialF(cmdEql),
			NewSymbol("equal"):                       SpecialF(cmdEqual),
			NewSymbol("equalp"):                      SpecialF(cmdEqualOp),
			NewSymbol("error-output"):                SpecialF(cmdErrorOutput),
			NewSymbol("evenp"):                       &Function{C: 1, F: funEvenp},
			NewSymbol("exit"):                        SpecialF(cmdQuit),
			NewSymbol("flet"):                        SpecialF(cmdFlet),
			NewSymbol("floatp"):                      &Function{C: 1, F: funAnyTypep[Float]},
			NewSymbol("floor"):                       &Function{C: 1, F: funFloor},
			NewSymbol("for"):                         SpecialF(cmdFor),
			NewSymbol("format"):                      defFormat,
			NewSymbol("format-char"):                 &Function{C: 2, F: funFormatChar},
			NewSymbol("format-float"):                &Function{C: 2, F: funFormatFloat},
			NewSymbol("format-integer"):              &Function{C: 3, F: funFormatInteger},
			NewSymbol("format-object"):               &Function{C: 3, F: funFormatObject},
			NewSymbol("funcall"):                     SpecialF(cmdFunCall),
			NewSymbol("function"):                    &Function{C: 1, F: funFunction},
			NewSymbol("functionp"):                   &Function{C: 1, F: funAnyTypep[Callable]},
			NewSymbol("get-output-stream-string"):    &Function{C: 1, F: funGetOutputStreamString},
			NewSymbol("if"):                          SpecialF(cmdIf),
			NewSymbol("incf"):                        SpecialF(cmdIncf),
			NewSymbol("integerp"):                    &Function{C: 1, F: funAnyTypep[Integer]},
			NewSymbol("labels"):                      SpecialF(cmdLabels),
			NewSymbol("lambda"):                      SpecialF(cmdLambda),
			NewSymbol("last"):                        &Function{C: 1, F: funLast},
			NewSymbol("length"):                      &Function{C: 1, F: funLength},
			NewSymbol("let"):                         SpecialF(cmdLet),
			NewSymbol("let*"):                        SpecialF(cmdLetX),
			NewSymbol("list"):                        &Function{C: -1, F: funList},
			NewSymbol("listp"):                       &Function{C: 1, F: funListp},
			NewSymbol("load"):                        &Function{C: 1, F: funLoad},
			NewSymbol("mapc"):                        &Function{C: -1, F: funMapC},
			NewSymbol("mapcan"):                      &Function{C: -1, F: funMapCan},
			NewSymbol("mapcar"):                      &Function{C: -1, F: funMapCar},
			NewSymbol("mapcon"):                      &Function{C: -1, F: funMapCon},
			NewSymbol("mapl"):                        &Function{C: -1, F: funMapL},
			NewSymbol("maplist"):                     &Function{C: -1, F: funMapList},
			NewSymbol("member"):                      &Function{C: 2, F: funMember},
			NewSymbol("minusp"):                      &Function{C: 1, F: funMinusp},
			NewSymbol("mod"):                         &Function{C: 2, F: funMod},
			NewSymbol("most-negative-fixnum"):        Integer(math.MinInt),
			NewSymbol("most-positive-fixnum"):        Integer(math.MaxInt),
			NewSymbol("nil"):                         Null,
			NewSymbol("not"):                         &Function{C: 1, F: funNot},
			NewSymbol("nreverse"):                    &Function{C: 1, F: funNReverse},
			NewSymbol("null"):                        &Function{C: 1, F: funNullp},
			NewSymbol("numberp"):                     &Function{C: 1, F: funNumberp},
			NewSymbol("oddp"):                        &Function{C: 1, F: funOddp},
			NewSymbol("open-input-file"):             &Function{C: 1, F: funOpenInputFile},
			NewSymbol("open-output-file"):            &Function{C: 1, F: funOpenOutputFile},
			NewSymbol("or"):                          SpecialF(cmdOr),
			NewSymbol("parse-number"):                &Function{C: 1, F: funParseNumber},
			NewSymbol("pi"):                          Float(math.Pi),
			NewSymbol("plusp"):                       &Function{C: 1, F: funPlusp},
			NewSymbol("probe-file"):                  &Function{C: 1, F: funProbeFile},
			NewSymbol("progn"):                       SpecialF(cmdProgn),
			NewSymbol("quit"):                        SpecialF(cmdQuit),
			NewSymbol("quote"):                       SpecialF(cmdQuote),
			NewSymbol("read"):                        defRead,
			NewSymbol("read-line"):                   defReadLine,
			NewSymbol("rem"):                         &Function{C: 2, F: funRem},
			NewSymbol("replaca"):                     &Function{C: 2, F: funReplaca},
			NewSymbol("replacd"):                     &Function{C: 2, F: funReplacd},
			NewSymbol("rest"):                        &LeftValueF{C: 1, F: funGetCdr},
			NewSymbol("return"):                      &Function{C: 1, F: funReturn},
			NewSymbol("return-from"):                 SpecialF(cmdReturnFrom),
			NewSymbol("reverse"):                     &Function{C: 1, F: funReverse},
			NewSymbol("round"):                       &Function{C: 1, F: funRound},
			NewSymbol("setf"):                        SpecialF(cmdSetf),
			NewSymbol("setq"):                        SpecialF(cmdSetq),
			NewSymbol("standard-input"):              SpecialF(cmdStandardInput),
			NewSymbol("standard-output"):             SpecialF(cmdStandardOutput),
			NewSymbol("string-append"):               &Function{C: -1, F: funStringAppend},
			NewSymbol("string-index"):                &Function{C: -1, F: funStringIndex},
			NewSymbol("string/="):                    &Function{C: 2, F: funStringNe},
			NewSymbol("string<"):                     &Function{C: 2, F: funStringLt},
			NewSymbol("string<="):                    &Function{C: 2, F: funStringLe},
			NewSymbol("string="):                     &Function{C: 2, F: funStringEq},
			NewSymbol("string>"):                     &Function{C: 2, F: funStringGt},
			NewSymbol("string>="):                    &Function{C: 2, F: funStringGe},
			NewSymbol("stringp"):                     &Function{C: 1, F: funAnyTypep[String]},
			NewSymbol("subseq"):                      &LeftValueF{C: 3, F: funSubSeq},
			NewSymbol("symbolp"):                     &Function{C: 1, F: funAnyTypep[Symbol]},
			NewSymbol("t"):                           True,
			NewSymbol("throw"):                       &Function{C: 2, F: funThrow},
			NewSymbol("trace"):                       SpecialF(cmdTrace),
			NewSymbol("truncate"):                    &Function{C: 1, F: funTruncate},
			NewSymbol("unwind-protect"):              SpecialF(cmdUnwindProtect),
			NewSymbol("vector"):                      &Function{C: -1, F: funVector},
			NewSymbol("while"):                       SpecialF(cmdWhile),
			NewSymbol("with-handler"):                SpecialF(cmdWithHandler),
			NewSymbol("with-open-input-file"):        SpecialF(cmdWithOpenInputFile),
			NewSymbol("with-open-output-file"):       SpecialF(cmdWithOpenOutputFile),
			NewSymbol("zerop"):                       &Function{C: 1, F: funZerop},
			// *sort*end*
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
	_f, ok := f.(Callable)
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
		return fmt.Sprintf("%s != %s (was %s)", equation, ToString(expect, PRINT), ToString(result, PRINT))
	}
	return ""
}
