package gmnlisp

import (
	"bufio"
	"bytes"
	"context"
	"embed"
	"errors"
	"fmt"
	"io"
	"math"
	"os"
	"strings"
	"sync"
)

var (
	symVariable        = NewSymbol("variable")
	symFunction        = NewReserved("function")
	symDynamicVariable = NewSymbol("dynamic-variable")
)

type Scope interface {
	Get(Symbol) (Node, bool)
	Set(Symbol, Node) error
	Range(func(Symbol, Node) bool)
}

type FuncScope interface {
	Get(Symbol) (Callable, bool)
	Set(Symbol, Callable)
	Range(func(Symbol, Callable) bool)
}

type Variables map[Symbol]Node

func (m Variables) Get(key Symbol) (Node, bool) {
	value, ok := m[key]
	return value, ok
}

func (m Variables) Set(key Symbol, value Node) error {
	m[key] = value
	return nil
}

type Constants map[Symbol]Node

func (m Constants) Get(key Symbol) (Node, bool) {
	value, ok := m[key]
	return value, ok
}

func (m Constants) Set(key Symbol, value Node) error {
	if _, ok := m[key]; ok {
		return fmt.Errorf("can't modify constant: %v", key)
	}
	m[key] = value
	return nil
}

func (m Variables) Range(f func(Symbol, Node) bool) {
	for key, val := range m {
		if !f(key, val) {
			return
		}
	}
}

type Functions map[Symbol]Callable

func (m Functions) Get(key Symbol) (Callable, bool) {
	value, ok := m[key]
	return value, ok
}

func (m Functions) Set(key Symbol, value Callable) {
	if value != nil {
		m[key] = value
	} else {
		delete(m, key)
	}
}

func (m Functions) Range(f func(Symbol, Callable) bool) {
	for key, val := range m {
		if !f(key, val) {
			return
		}
	}
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

func (m *Pair) Set(key Symbol, value Node) error {
	if key == m.Key {
		m.Value = value
		return nil
	}
	return errors.New("Pair can be set value")
}

func (m *Pair) Range(f func(Symbol, Node) bool) {
	f(m.Key, m.Value)
}

type Writer interface {
	io.Writer
	Node
}

type shared struct {
	macro     map[Symbol]*_Macro
	handler   []Callable
	global    Scope
	defun     FuncScope
	dynamic   Variables
	constants Constants
	stdout    interface {
		io.Writer
		Node
	}
	errout interface {
		io.Writer
		Node
	}
	stdin interface {
		_Reader
		Node
	}
	startup    sync.Once
	blockName  map[int]struct{}
	catchTag   map[Node]struct{}
	goTag      map[Symbol]struct{}
	StrictMode bool
	class      map[Symbol]Class
}

type World struct {
	*shared
	parent *World
	funcs  FuncScope
	vars   Scope
	aux    any
}

type _Reader interface {
	io.RuneScanner
	io.ByteReader
	io.Reader
}

func (w *World) Get(name Symbol) (Node, error) {
	for W := w; W != nil; {
		if W.vars != nil {
			if value, ok := W.vars.Get(name); ok {
				return value, nil
			}
		}
		W = W.parent
	}
	if value, ok := w.constants.Get(name); ok {
		return value, nil
	}
	return Null, &_UndefinedEntity{name: name, space: symVariable}
}

func (w *World) GetFunc(name Symbol) (Callable, error) {
	for W := w; W != nil; W = W.parent {
		if W.funcs != nil {
			if value, ok := W.funcs.Get(name); ok {
				return value, nil
			}
		}
	}
	if value, ok := w.shared.defun.Get(name); ok {
		return value, nil
	}
	return nil, &_UndefinedEntity{name: name, space: symFunction}
}

// DefineGlobal implements (defglobal) of ISLisp or (defparameter) of CommonLisp.
func (w *World) DefineGlobal(name Symbol, value Node) {
	w.global.Set(name, value)
}

func (w *World) Set(name Symbol, value Node) error {
	for w != nil {
		if w.vars != nil {
			if _, ok := w.vars.Get(name); ok {
				if err := w.vars.Set(name, value); err == nil {
					return nil
				}
			}
		}
		w = w.parent
	}
	return &_UndefinedEntity{name: name, space: symVariable}
}

func funStandardOutput(ctx context.Context, w *World) (Node, error) {
	return w.stdout, nil
}

func (w *World) Stdout() io.Writer {
	return w.stdout
}

func (w *World) SetStdout(writer io.Writer) {
	if f, ok := writer.(*os.File); ok {
		w.stdout = newOutputFileStream(f)
	} else if s, ok := writer.(*WriterStream); ok {
		w.stdout = s
	} else {
		w.stdout = &WriterStream{writer: writer}
	}
}

func funErrorOutput(ctx context.Context, w *World) (Node, error) {
	return w.errout, nil
}

func (w *World) Errout() io.Writer {
	return w.errout
}

func (w *World) SetErrout(writer io.Writer) {
	if f, ok := writer.(*os.File); ok {
		w.errout = newOutputFileStream(f)
	} else if s, ok := writer.(*WriterStream); ok {
		w.errout = s
	} else {
		w.errout = &WriterStream{writer: writer}
	}
}

func funStandardInput(ctx context.Context, w *World) (Node, error) {
	return w.stdin, nil
}

func (w *World) Stdin() _Reader {
	return w.stdin
}

var autoLoadVars = Variables{}

var autoLoadConstants = Constants{
	// *sort*start*
	NewReserved("*most-negative-fixnum*"): Integer(math.MinInt),
	NewReserved("*most-negative-float*"):  Float(-math.MaxFloat64),
	NewReserved("*most-positive-fixnum*"): Integer(math.MaxInt),
	NewReserved("*most-positive-float*"):  Float(math.MaxFloat64),
	NewReserved("*pi*"):                   Float(math.Pi),
	NewSymbol("*err-exist*"):              ErrorNode{Value: os.ErrExist},
	NewSymbol("*err-not-exist*"):          ErrorNode{Value: os.ErrNotExist},
	NewSymbol("*err-quit*"):               ErrorNode{Value: ErrQuit},
	NewSymbol("*err-too-few-arguments*"):  ErrTooFewArguments,
	NewSymbol("*err-too-many-arguments*"): ErrTooManyArguments,
	NewSymbol("*err-too-short-tokens*"):   ErrorNode{Value: ErrTooShortTokens},
	// *sort*end*
}

var autoLoadFunc = Functions{
	// *sort*start*
	NewReserved("and"):                          SpecialF(cmdAnd),
	NewReserved("assure"):                       SpecialF(cmdAssure),
	NewReserved("block"):                        SpecialF(cmdBlock),
	NewReserved("case"):                         SpecialF(cmdCase),
	NewReserved("catch"):                        SpecialF(cmdCatch),
	NewReserved("class"):                        SpecialF(cmdClass),
	NewReserved("cond"):                         SpecialF(cmdCond),
	NewReserved("convert"):                      SpecialF(cmdConvert),
	NewReserved("dynamic"):                      SpecialF(cmdDynamic),
	NewReserved("dynamic-let"):                  SpecialF(cmdDynamicLet),
	NewReserved("flet"):                         SpecialF(cmdFlet),
	NewReserved("function"):                     SpecialF(cmdFunction),
	NewReserved("go"):                           SpecialF(cmdGo),
	NewReserved("if"):                           &SpecialN{F: cmdIf, Min: 2, Max: 3},
	NewReserved("ignore-errors"):                SpecialF(cmdIgnoreErrors),
	NewReserved("labels"):                       SpecialF(cmdLabels),
	NewReserved("lambda"):                       SpecialF(cmdLambda),
	NewReserved("let"):                          SpecialF(cmdLet),
	NewReserved("let*"):                         SpecialF(cmdLetX),
	NewReserved("or"):                           SpecialF(cmdOr),
	NewReserved("progn"):                        SpecialF(cmdProgn),
	NewReserved("quote"):                        SpecialF(cmdQuote),
	NewReserved("return-from"):                  SpecialF(cmdReturnFrom),
	NewReserved("tagbody"):                      SpecialF(cmdTagBody),
	NewReserved("the"):                          SpecialF(cmdAssure),
	NewReserved("throw"):                        Function2(funThrow),
	NewReserved("unwind-protect"):               SpecialF(cmdUnwindProtect),
	NewReserved("while"):                        SpecialF(cmdWhile),
	NewReserved("with-error-output"):            SpecialF(cmdWithErrorOutput),
	NewReserved("with-handler"):                 SpecialF(cmdWithHandler),
	NewReserved("with-open-input-file"):         SpecialF(cmdWithOpenInputFile),
	NewReserved("with-open-output-file"):        SpecialF(cmdWithOpenOutputFile),
	NewReserved("with-standard-input"):          SpecialF(cmdWithStandardInput),
	NewReserved("with-standard-output"):         SpecialF(cmdWithStandardOutput),
	NewSymbol("%make-simple-error"):             Function2(funMakeSimpleError),
	NewSymbol("%raise-domain-error"):            Function2(funRaiseDomainError),
	NewSymbol("*"):                              &Function{F: funMulti},
	NewSymbol("+"):                              &Function{F: funAdd},
	NewSymbol("-"):                              &Function{Min: 1, F: funSub},
	NewSymbol("/="):                             Function2(funNotEqual),
	NewSymbol("<"):                              &Function{C: 2, F: funLessThan},
	NewSymbol("<="):                             &Function{C: 2, F: funLessOrEqual},
	NewSymbol("="):                              Function2(funEqualOp),
	NewSymbol(">"):                              &Function{C: 2, F: funGreaterThan},
	NewSymbol(">="):                             &Function{C: 2, F: funGreaterOrEqual},
	NewSymbol("abort"):                          Function0(funAbort),
	NewSymbol("append"):                         &Function{F: funAppend},
	NewSymbol("apply"):                          SpecialF(cmdApply),
	NewSymbol("aref"):                           &Function{Min: 1, F: funAref},
	NewSymbol("arithmetic-error-operands"):      Function1(funArithmeticErrorOperands),
	NewSymbol("arithmetic-error-operation"):     Function1(funArithmeticErrorOperation),
	NewSymbol("array-dimensions"):               Function1(funArrayDimensions),
	NewSymbol("assoc"):                          Function2(Assoc),
	NewSymbol("atan"):                           funMath1(math.Atan),
	NewSymbol("atan2"):                          Function2(funAtan2),
	NewSymbol("atanh"):                          funMath1(math.Atanh),
	NewSymbol("atom"):                           Function1(funAtom),
	NewSymbol("basic-array*-p"):                 Function1(funBasicArrayStarP),
	NewSymbol("basic-array-p"):                  Function1(funBasicArrayP),
	NewSymbol("basic-vector-p"):                 Function1(funBasicVectorP),
	NewSymbol("car"):                            Function1(funGetCar),
	NewSymbol("cdr"):                            Function1(funGetCdr),
	NewSymbol("ceiling"):                        Function1(funCeiling),
	NewSymbol("char-index"):                     &Function{Min: 2, Max: 3, F: funRuneIndex},
	NewSymbol("char/="):                         &Function{C: 2, F: funRuneNe},
	NewSymbol("char<"):                          &Function{C: 2, F: funRuneLt},
	NewSymbol("char<="):                         &Function{C: 2, F: funRuneLe},
	NewSymbol("char="):                          &Function{C: 2, F: funRuneEq},
	NewSymbol("char>"):                          &Function{C: 2, F: funRuneGt},
	NewSymbol("char>="):                         &Function{C: 2, F: funRuneGe},
	NewSymbol("characterp"):                     Function1(funAnyTypep[Rune]),
	NewSymbol("class-of"):                       Function1(funClassOf),
	NewSymbol("close"):                          Function1(funClose),
	NewSymbol("clrhash"):                        Function1(funClearHash),
	NewSymbol("condition-continuable"):          Function1(funConditionContinuable),
	NewSymbol("cons"):                           Function2(funCons),
	NewSymbol("consp"):                          Function1(funAnyTypep[*Cons]),
	NewSymbol("continue-condition"):             &Function{Min: 1, Max: 2, F: funContinueCondition},
	NewSymbol("cos"):                            funMath1(math.Cos),
	NewSymbol("cosh"):                           funMath1(math.Cosh),
	NewSymbol("create"):                         &Function{Min: 1, F: funCreate},
	NewSymbol("create-array"):                   &Function{Min: 1, Max: 2, F: funCreateArray},
	NewSymbol("create-list"):                    &Function{Min: 1, Max: 2, F: funCreateList},
	NewSymbol("create-string"):                  &Function{Min: 1, Max: 2, F: funCreateString},
	NewSymbol("create-string-input-stream"):     Function1(funCreateStringInputStream),
	NewSymbol("create-string-output-stream"):    Function0(funCreateStringOutputStream),
	NewSymbol("create-vector"):                  &Function{Min: 1, Max: 2, F: funCreateVector},
	NewSymbol("defclass"):                       SpecialF(cmdDefClass),
	NewSymbol("defconstant"):                    SpecialF(cmdDefConstant),
	NewSymbol("defdynamic"):                     SpecialF(cmdDefDynamic),
	NewSymbol("defgeneric"):                     SpecialF(cmdDefGeneric),
	NewSymbol("defglobal"):                      SpecialF(cmdDefglobal),
	NewSymbol("defmacro"):                       SpecialF(cmdDefMacro),
	NewSymbol("defmethod"):                      SpecialF(cmdDefMethod),
	NewSymbol("defun"):                          SpecialF(cmdDefun),
	NewSymbol("div"):                            &Function{C: 2, F: funDivide},
	NewSymbol("domain-error-expected-class"):    Function1(funDomainErrorExpectedClass),
	NewSymbol("domain-error-object"):            Function1(funDomainErrorObject),
	NewSymbol("elt"):                            &Function{C: 2, F: funElt},
	NewSymbol("eq"):                             &Function{C: 2, F: funEq},
	NewSymbol("eql"):                            &Function{C: 2, F: funEql},
	NewSymbol("equal"):                          &Function{C: 2, F: funEqual},
	NewSymbol("equalp"):                         &Function{C: 2, F: funEqualp},
	NewSymbol("error-output"):                   Function0(funErrorOutput),
	NewSymbol("evenp"):                          Function1(funEvenp),
	NewSymbol("exit"):                           Function0(funQuit),
	NewSymbol("exp"):                            funMath1(math.Exp),
	NewSymbol("expand-defun"):                   SpecialF(cmdExpandDefun),
	NewSymbol("expt"):                           Function2(funExpt),
	NewSymbol("file-length"):                    Function2(funFileLength),
	NewSymbol("file-position"):                  Function1(funFilePosition),
	NewSymbol("finish-output"):                  Function1(funFinishOutput),
	NewSymbol("float"):                          Function1(funFloat),
	NewSymbol("floatp"):                         Function1(funAnyTypep[Float]),
	NewSymbol("floor"):                          Function1(funFloor),
	NewSymbol("format"):                         &Function{Min: 2, F: funFormat},
	NewSymbol("format-char"):                    &Function{C: 2, F: funFormatChar},
	NewSymbol("format-float"):                   &Function{C: 2, F: funFormatFloat},
	NewSymbol("format-fresh-line"):              Function1(funFormatFreshLine),
	NewSymbol("format-integer"):                 &Function{C: 3, F: funFormatInteger},
	NewSymbol("format-object"):                  &Function{C: 3, F: funFormatObject},
	NewSymbol("format-tab"):                     Function2(funFormatTab),
	NewSymbol("funcall"):                        SpecialF(cmdFunCall),
	NewSymbol("functionp"):                      Function1(funAnyTypep[FunctionRef]),
	NewSymbol("garef"):                          &Function{Min: 1, F: funGaref},
	NewSymbol("general-array*-p"):               Function1(funGeneralArrayStarP),
	NewSymbol("general-array-p"):                Function1(funAnyTypep[*Array]),
	NewSymbol("general-vector-p"):               Function1(funGeneralVectorP),
	NewSymbol("generic-function-p"):             Function1(funGenericFunctionP),
	NewSymbol("gensym"):                         Function0(funGensym),
	NewSymbol("get-internal-real-time"):         Function0(funInternalRealTime),
	NewSymbol("get-internal-run-time"):          Function0(funInternalRealTime),
	NewSymbol("get-output-stream-string"):       Function1(funGetOutputStreamString),
	NewSymbol("get-universal-time"):             Function0(funUniversalTime),
	NewSymbol("gethash"):                        Function2(funGetHash),
	NewSymbol("gmn:dump-session"):               Function0(funDumpSession),
	NewSymbol("hash-table-count"):               Function1(funHashTableCount),
	NewSymbol("identity"):                       Function1(funIdentity),
	NewSymbol("input-stream-p"):                 Function1(funInputStreamP),
	NewSymbol("instancep"):                      Function2(funInstanceP),
	NewSymbol("integerp"):                       Function1(funAnyTypep[Integer]),
	NewSymbol("internal-time-units-per-second"): Function0(funInternalTimeUnitPerSecond),
	NewSymbol("isqrt"):                          Function1(funIsqrt),
	NewSymbol("lambda-macro"):                   SpecialF(cmdLambdaMacro),
	NewSymbol("last"):                           Function1(funLast),
	NewSymbol("length"):                         Function1(funLength),
	NewSymbol("list"):                           &Function{F: funList},
	NewSymbol("listp"):                          Function1(funListp),
	NewSymbol("log"):                            Function1(funLog),
	NewSymbol("macroexpand"):                    Function1(funMacroExpand),
	NewSymbol("make-hash-table"):                Function0(funMakeHashTable),
	NewSymbol("mapc"):                           &Function{Min: 2, F: funMapC},
	NewSymbol("mapcan"):                         &Function{Min: 2, F: funMapCan},
	NewSymbol("mapcar"):                         &Function{Min: 2, F: funMapCar},
	NewSymbol("mapcon"):                         &Function{Min: 2, F: funMapCon},
	NewSymbol("mapl"):                           &Function{Min: 2, F: funMapL},
	NewSymbol("maplist"):                        &Function{Min: 2, F: funMapList},
	NewSymbol("member"):                         Function2(funMember),
	NewSymbol("minusp"):                         Function1(funMinusp),
	NewSymbol("mod"):                            &Function{C: 2, F: funMod},
	NewSymbol("not"):                            Function1(funNot),
	NewSymbol("nreverse"):                       Function1(NReverse),
	NewSymbol("null"):                           Function1(funNullp),
	NewSymbol("numberp"):                        Function1(funNumberp),
	NewSymbol("oddp"):                           Function1(funOddp),
	NewSymbol("open-input-file"):                &Function{Min: 1, Max: 2, F: funOpenInputFile},
	NewSymbol("open-io-file"):                   &Function{Min: 1, Max: 2, F: funOpenIoFile},
	NewSymbol("open-output-file"):               &Function{Min: 1, Max: 2, F: funOpenOutputFile},
	NewSymbol("open-stream-p"):                  Function1(funOpenStreamP),
	NewSymbol("output-stream-p"):                Function1(funOutputStreamP),
	NewSymbol("parse-error-expected-class"):     Function1(funParseErrorExpectedClass),
	NewSymbol("parse-error-string"):             Function1(funParseErrorString),
	NewSymbol("parse-number"):                   Function1(funParseNumber),
	NewSymbol("plusp"):                          Function1(funPlusp),
	NewSymbol("preview-char"):                   &Function{Max: 3, F: funPreviewChar},
	NewSymbol("probe-file"):                     Function1(funProbeFile),
	NewSymbol("psetq"):                          SpecialF(cmdPSetq),
	NewSymbol("quasiquote"):                     SpecialF(cmdQuasiQuote),
	NewSymbol("quit"):                           Function0(funQuit),
	NewSymbol("quotient"):                       &Function{Min: 2, F: funQuotient},
	NewSymbol("read"):                           &Function{Max: 3, F: funRead},
	NewSymbol("read-byte"):                      &Function{Min: 1, Max: 3, F: funReadByte},
	NewSymbol("read-char"):                      &Function{Max: 3, F: funReadChar},
	NewSymbol("read-line"):                      &Function{Max: 3, F: funReadLine},
	NewSymbol("reciprocal"):                     Function1(funReciprocal),
	NewSymbol("rem"):                            Function2(funRem),
	NewSymbol("remhash"):                        Function2(funRemoveHash),
	NewSymbol("rest"):                           Function1(funGetCdr),
	NewSymbol("reverse"):                        Function1(funReverse),
	NewSymbol("round"):                          Function1(funRound),
	NewSymbol("set-aref"):                       &Function{Min: 3, F: funSetAref},
	NewSymbol("set-car"):                        Function2(funSetCar),
	NewSymbol("set-cdr"):                        Function2(funSetCdr),
	NewSymbol("set-file-position"):              Function2(funSetFilePosition),
	NewSymbol("set-garef"):                      &Function{Min: 3, F: funSetGaref},
	NewSymbol("set-gethash"):                    &Function{C: 3, F: funSetHash},
	NewSymbol("setq"):                           &SpecialN{F: cmdSetq, Min: 2, Max: 2},
	NewSymbol("signal-condition"):               Function2(funSignalCondition),
	NewSymbol("simple-error-format-arguments"):  Function1(funSimpleErrorFormatArguments),
	NewSymbol("simple-error-format-string"):     Function1(funSimpleErrorFormatString),
	NewSymbol("sin"):                            funMath1(math.Sin),
	NewSymbol("sinh"):                           funMath1(math.Sinh),
	NewSymbol("sqrt"):                           Function1(funSqrt),
	NewSymbol("standard-input"):                 Function0(funStandardInput),
	NewSymbol("standard-output"):                Function0(funStandardOutput),
	NewSymbol("stream-error-stream"):            Function1(funStreamErrorStream),
	NewSymbol("stream-ready-p"):                 Function1(funStreamReadyP),
	NewSymbol("streamp"):                        Function1(funStreamP),
	NewSymbol("string-append"):                  &Function{F: funStringAppend},
	NewSymbol("string-index"):                   &Function{Min: 2, Max: 3, F: funStringIndex},
	NewSymbol("string/="):                       &Function{C: 2, F: funStringNe},
	NewSymbol("string<"):                        &Function{C: 2, F: funStringLt},
	NewSymbol("string<="):                       &Function{C: 2, F: funStringLe},
	NewSymbol("string="):                        &Function{C: 2, F: funStringEq},
	NewSymbol("string>"):                        &Function{C: 2, F: funStringGt},
	NewSymbol("string>="):                       &Function{C: 2, F: funStringGe},
	NewSymbol("stringp"):                        Function1(funAnyTypep[String]),
	NewSymbol("subclassp"):                      Function2(funSubClassP),
	NewSymbol("subseq"):                         &Function{C: 3, F: funSubSeq},
	NewSymbol("symbolp"):                        Function1(funAnyTypep[Symbol]),
	NewSymbol("tan"):                            funMath1(math.Tan),
	NewSymbol("tanh"):                           funMath1(math.Tanh),
	NewSymbol("trace"):                          SpecialF(cmdTrace),
	NewSymbol("truncate"):                       Function1(funTruncate),
	NewSymbol("undefined-entity-name"):          Function1(funUndefinedEntityName),
	NewSymbol("undefined-entity-namespace"):     Function1(funUndefinedEntityNamespace),
	NewSymbol("vector"):                         &Function{F: funVector},
	NewSymbol("with-open-io-file"):              SpecialF(cmdWithOpenIoFile),
	NewSymbol("write-byte"):                     Function2(funWriteByte),
	NewSymbol("zerop"):                          Function1(funZerop),
	symReportCondition:                          reportCondition,
	// *sort*end*
}

func Export(name Symbol, value Callable) {
	autoLoadFunc[name] = value
}

func ExportRange(v Functions) {
	for key, val := range v {
		autoLoadFunc[key] = val
	}
}

//go:embed embed/*
var embedLisp embed.FS

type _RootWorld map[Symbol]Callable

func (rw _RootWorld) Get(symbol Symbol) (Callable, bool) {
	if value, ok := rw[symbol]; ok {
		return value, true
	}
	if value, ok := autoLoadFunc[symbol]; ok {
		return value, true
	}
	fname := "embed/" + strings.ToLower(symbol.String()) + ".lsp"

	script, err := embedLisp.ReadFile(fname)
	if err == nil {
		value := &LispString{S: string(script)}
		autoLoadFunc[symbol] = value
		return value, true
	}
	return nil, false
}

func (rw _RootWorld) Set(symbol Symbol, value Callable) {
	rw[symbol] = value
}

func (rw _RootWorld) Range(f func(Symbol, Callable) bool) {
	for key, val := range rw {
		if !f(key, val) {
			break
		}
	}
}

var presetClass = []Class{
	BuiltInClassObject,
	numberClass,
	ObjectClass,
	standardClass,
}

func New() *World {
	rwvars := &autoLoadVars
	rwfuncs := _RootWorld{}
	w := &World{
		shared: &shared{
			global:    rwvars,
			defun:     rwfuncs,
			dynamic:   Variables{},
			constants: autoLoadConstants,
			stdin:     &inputStream{_Reader: bufio.NewReader(os.Stdin), file: os.Stdin},
			stdout:    &WriterStream{writer: os.Stdout},
			errout:    &WriterStream{writer: os.Stderr},
			class:     map[Symbol]Class{},
		},
		vars:  rwvars,
		funcs: rwfuncs,
	}
	for _, c := range presetClass {
		w.class[c.Name()] = c
	}
	return w
}

func (w *World) Eval(ctx context.Context, node Node) (Node, error) {
	type canEval interface {
		Eval(context.Context, *World) (Node, error)
	}
	if e, ok := node.(canEval); ok {
		return e.Eval(ctx, w)
	}
	return node, nil
}

func (w *World) ShiftAndEvalCar(ctx context.Context, list Node) (Node, Node, error) {
	first, list, err := Shift(list)
	if err != nil {
		_, err = raiseProgramError(ctx, w, err)
		return nil, list, err
	}
	value, err := w.Eval(ctx, first)
	if err != nil {
		return nil, list, err
	}
	return value, list, nil
}

func inject(list []Node, f func(left, right Node) (Node, error)) (Node, error) {
	if len(list) <= 0 {
		return Null, nil
	}
	result, list := list[0], list[1:]
	for len(list) > 0 {
		var err error
		result, err = f(result, list[0])
		if err != nil {
			return nil, err
		}
		list = list[1:]
	}
	return result, nil
}

//go:embed startup.lsp
var startupCode string

func (w *World) InterpretNodes(ctx context.Context, ns []Node) (Node, error) {
	w.startup.Do(func() {
		compiled, err := ReadAll(strings.NewReader(startupCode))
		if err != nil {
			panic(err.Error())
		}
		ns = append(compiled, ns...)
	})
	var result Node = Null
	var err error

	defer func() {
		if f, ok := w.stdout.(interface{ Flush() error }); ok {
			f.Flush()
		}
		if f, ok := w.errout.(interface{ Flush() error }); ok {
			f.Flush()
		}
	}()

	for _, c := range ns {
		result, err = w.Eval(ctx, c)
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

func (w *World) Let(scope Scope) *World {
	return &World{
		parent: w,
		vars:   scope,
		funcs:  nil,
		shared: w.shared,
	}
}

func (w *World) Flet(scope FuncScope) *World {
	return &World{
		parent: w,
		vars:   nil,
		funcs:  scope,
		shared: w.shared,
	}
}

func (w *World) Assert(equation string, expect Node) string {
	result, err := w.Interpret(context.TODO(), equation)
	if err != nil {
		return fmt.Sprintf("%#v: %#v", equation, err.Error())
	}
	if !result.Equals(expect, EQUAL) {
		return fmt.Sprintf("%#v != %#v (was %#v)", equation, expect, result)
	}
	return ""
}

func (W *World) Range(f func(Symbol, Node) bool) {
	marked := map[Symbol]struct{}{}
	callback := func(key Symbol, val Node) bool {
		if _, ok := marked[key]; !ok {
			if !f(key, val) {
				return false
			}
			marked[key] = struct{}{}
		}
		return true
	}
	for w := W; w != nil; w = w.parent {
		w.vars.Range(callback)
	}
	W.global.Range(callback)
}

func (W *World) FuncRange(f func(Symbol, Callable) bool) {
	marked := map[Symbol]struct{}{}
	callback := func(key Symbol, val Callable) bool {
		if _, ok := marked[key]; !ok {
			if !f(key, val) {
				return false
			}
			marked[key] = struct{}{}
		}
		return true
	}
	for w := W; w != nil; w = w.parent {
		w.funcs.Range(callback)
	}
	W.defun.Range(callback)
	autoLoadFunc.Range(callback)
}

func funDumpSession(_ context.Context, w *World) (Node, error) {
	out := w.stdout
	var err error
	w.Range(func(key Symbol, val Node) bool {
		cons := &Cons{
			Car: key,
			Cdr: &Cons{
				Car: val,
				Cdr: Null,
			},
		}
		_, err = cons.PrintTo(out, PRINT)
		if err != nil {
			return false
		}
		_, err = fmt.Fprintln(out)
		return err == nil
	})
	return True, err
}
