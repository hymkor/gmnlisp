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
			NewSymbol("cadddr"):                      &LeftValueF{C: 1, F: funCadddr},
			NewSymbol("caddr"):                       &LeftValueF{C: 1, F: funCaddr},
			NewSymbol("cadr"):                        &LeftValueF{C: 1, F: funCadr},
			NewSymbol("car"):                         &LeftValueF{C: 1, F: funGetCar},
			NewSymbol("case"):                        SpecialF(cmdCase),
			NewSymbol("catch"):                       SpecialF(cmdCatch),
			NewSymbol("cdddr"):                       &LeftValueF{C: 1, F: funCdddr},
			NewSymbol("cddr"):                        &LeftValueF{C: 1, F: funCddr},
			NewSymbol("cdr"):                         &LeftValueF{C: 1, F: funGetCdr},
			NewSymbol("close"):                       &Function{C: 1, F: funClose},
			NewSymbol("coerce"):                      &Function{C: 2, F: funCoerce},
			NewSymbol("command"):                     defCommand,
			NewSymbol("concatenate"):                 defConcatenate,
			NewSymbol("cond"):                        SpecialF(cmdCond),
			NewSymbol("cons"):                        &Function{C: 2, F: funCons},
			NewSymbol("consp"):                       &Function{C: 1, F: funAnyTypep[*Cons]},
			NewSymbol("create-string-input-stream"):  &Function{C: 1, F: funCreateStringInputStream},
			NewSymbol("create-string-output-stream"): SpecialF(cmdCreateStringOutputStream),
			NewSymbol("defdynamic"):                  SpecialF(cmdDefDynamic),
			NewSymbol("decf"):                        SpecialF(cmdDecf),
			NewSymbol("defglobal"):                   SpecialF(cmdDefglobal),
			NewSymbol("defmacro"):                    SpecialF(cmdDefMacro),
			NewSymbol("defun"):                       SpecialF(cmdDefun),
			NewSymbol("dolist"):                      SpecialF(cmdDoList),
			NewSymbol("dotimes"):                     SpecialF(cmdDoTimes),
			NewSymbol("dynamic"):                     SpecialF(cmdDynamic),
			NewSymbol("dynamic-let"):                 SpecialF(cmdDynamicLet),
			NewSymbol("elt"):                         &LeftValueF{C: 2, F: funElt},
			NewSymbol("eq"):                          SpecialF(cmdEq),
			NewSymbol("eql"):                         SpecialF(cmdEql),
			NewSymbol("equal"):                       SpecialF(cmdEqual),
			NewSymbol("equalp"):                      SpecialF(cmdEqualOp),
			NewSymbol("error-output"):                SpecialF(cmdErrorOutput),
			NewSymbol("evenp"):                       &Function{C: 1, F: funEvenp},
			NewSymbol("exit"):                        SpecialF(cmdQuit),
			NewSymbol("find"):                        &KWFunction{C: 2, F: funFind},
			NewSymbol("first"):                       &LeftValueF{C: 1, F: funGetCar},
			NewSymbol("floatp"):                      &Function{C: 1, F: funAnyTypep[Float]},
			NewSymbol("for"):                         SpecialF(cmdFor),
			NewSymbol("foreach"):                     SpecialF(cmdForeach),
			NewSymbol("format"):                      defFormat,
			NewSymbol("format-char"):                 &Function{C: 2, F: funFormatChar},
			NewSymbol("format-integer"):              &Function{C: 3, F: funFormatInteger},
			NewSymbol("format-object"):               &Function{C: 3, F: funFormatObject},
			NewSymbol("funcall"):                     SpecialF(cmdFunCall),
			NewSymbol("function"):                    &Function{C: 1, F: funFunction},
			NewSymbol("get-output-stream-string"):    &Function{C: 1, F: funGetOutputStreamString},
			NewSymbol("if"):                          SpecialF(cmdIf),
			NewSymbol("incf"):                        SpecialF(cmdIncf),
			NewSymbol("integerp"):                    &Function{C: 1, F: funAnyTypep[Integer]},
			NewSymbol("lambda"):                      SpecialF(cmdLambda),
			NewSymbol("last"):                        &Function{C: 1, F: funLast},
			NewSymbol("length"):                      &Function{C: 1, F: funLength},
			NewSymbol("let"):                         SpecialF(cmdLet),
			NewSymbol("let*"):                        SpecialF(cmdLetX),
			NewSymbol("list"):                        &Function{C: -1, F: funList},
			NewSymbol("listp"):                       &Function{C: 1, F: funListp},
			NewSymbol("load"):                        &Function{C: 1, F: funLoad},
			NewSymbol("macroexpand"):                 SpecialF(cmdMacroExpand),
			NewSymbol("map"):                         &Function{C: -1, F: funMap},
			NewSymbol("mapc"):                        &Function{C: -1, F: funMapC},
			NewSymbol("mapcar"):                      &Function{C: -1, F: funMapCar},
			NewSymbol("mapcan"):                      &Function{C: -1, F: funMapCan},
			NewSymbol("mapcon"):                      &Function{C: -1, F: funMapCon},
			NewSymbol("mapl"):                        &Function{C: -1, F: funMapL},
			NewSymbol("maplist"):                     &Function{C: -1, F: funMapList},
			NewSymbol("member"):                      &KWFunction{C: 2, F: funMember},
			NewSymbol("minusp"):                      &Function{C: 1, F: funMinusp},
			NewSymbol("mod"):                         &Function{C: 2, F: funMod},
			NewSymbol("most-negative-fixnum"):        Integer(math.MinInt),
			NewSymbol("most-positive-fixnum"):        Integer(math.MaxInt),
			NewSymbol("nil"):                         Null,
			NewSymbol("not"):                         &Function{C: 1, F: funNot},
			NewSymbol("nth"):                         &LeftValueF{C: 2, F: funNth},
			NewSymbol("nthcdr"):                      &LeftValueF{C: 2, F: funNthcdr},
			NewSymbol("null"):                        &Function{C: 1, F: funNullp},
			NewSymbol("numberp"):                     &Function{C: 1, F: funNumberp},
			NewSymbol("oddp"):                        &Function{C: 1, F: funOddp},
			NewSymbol("open"):                        SpecialF(cmdOpen),
			NewSymbol("or"):                          SpecialF(cmdOr),
			NewSymbol("parse-integer"):               &Function{C: 1, F: funParseInt},
			NewSymbol("pi"):                          Float(math.Pi),
			NewSymbol("plusp"):                       &Function{C: 1, F: funPlusp},
			NewSymbol("position"):                    &KWFunction{C: 2, F: funPosition},
			NewSymbol("progn"):                       SpecialF(cmdProgn),
			NewSymbol("quit"):                        SpecialF(cmdQuit),
			NewSymbol("quote"):                       SpecialF(cmdQuote),
			NewSymbol("read"):                        defRead,
			NewSymbol("read-from-string"):            &Function{C: 1, F: funReadFromString},
			NewSymbol("read-line"):                   defReadLine,
			NewSymbol("rem"):                         &Function{C: 2, F: funRem},
			NewSymbol("replaca"):                     &Function{C: 2, F: funReplaca},
			NewSymbol("replacd"):                     &Function{C: 2, F: funReplacd},
			NewSymbol("rest"):                        &LeftValueF{C: 1, F: funGetCdr},
			NewSymbol("return"):                      &Function{C: 1, F: funReturn},
			NewSymbol("return-from"):                 SpecialF(cmdReturnFrom),
			NewSymbol("reverse"):                     &Function{C: 1, F: funReverse},
			NewSymbol("second"):                      &LeftValueF{C: 1, F: funCadr},
			NewSymbol("setf"):                        SpecialF(cmdSetf),
			NewSymbol("setq"):                        SpecialF(cmdSetq),
			NewSymbol("standard-input"):              SpecialF(cmdStandardInput),
			NewSymbol("standard-output"):             SpecialF(cmdStandardOutput),
			NewSymbol("string-append"):               &Function{C: -1, F: funStringAppend},
			NewSymbol("stringp"):                     &Function{C: 1, F: funAnyTypep[String]},
			NewSymbol("subseq"):                      &LeftValueF{C: -1, F: funSubSeq},
			NewSymbol("subst"):                       &Function{C: 3, F: funSubst},
			NewSymbol("symbolp"):                     &Function{C: 1, F: funAnyTypep[Symbol]},
			NewSymbol("t"):                           True,
			NewSymbol("third"):                       &LeftValueF{C: 1, F: funCaddr},
			NewSymbol("throw"):                       &Function{C: 2, F: funThrow},
			NewSymbol("to-utf32"):                    &Function{C: 1, F: funToUTF32},
			NewSymbol("to-utf8"):                     &Function{C: 1, F: funToUTF8},
			NewSymbol("trace"):                       SpecialF(cmdTrace),
			NewSymbol("truncate"):                    &Function{C: 1, F: funTruncate},
			NewSymbol("typep"):                       &Function{C: 2, F: funTypep},
			NewSymbol("unless"):                      SpecialF(cmdUnless),
			NewSymbol("unwind-protect"):              SpecialF(cmdUnwindProtect),
			NewSymbol("when"):                        SpecialF(cmdWhen),
			NewSymbol("while"):                       SpecialF(cmdWhile),
			NewSymbol("with-handler"):                SpecialF(cmdWithHandler),
			NewSymbol("with-open-file"):              SpecialF(cmdWithOpenFile),
			NewSymbol("zerop"):                       &Function{C: 1, F: funZerop},
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
		return fmt.Sprintf("%s != %s (was %s)", equation, ToString(expect, PRINT), ToString(result, PRINT))
	}
	return ""
}
