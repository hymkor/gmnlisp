package gmnlisp

import (
	"bufio"
	"bytes"
	"context"
	"embed"
	"fmt"
	"io"
	"math"
	"os"
	"strings"
	"sync"
	"unicode/utf8"
)

type Scope interface {
	Get(Symbol) (Node, bool)
	Set(Symbol, Node)
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

func (m Variables) Set(key Symbol, value Node) {
	m[key] = value
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

func (m *Pair) Set(key Symbol, value Node) {
	if key == m.Key {
		m.Value = value
	}
	panic("Pair can be set value")
}

func (m *Pair) Range(f func(Symbol, Node) bool) {
	f(m.Key, m.Value)
}

type shared struct {
	handler Callable
	global  Scope
	defun   FuncScope
	dynamic Variables
	stdout  *_WriterNode
	errout  *_WriterNode
	stdin   _ReaderNode
	startup sync.Once
}

type World struct {
	*shared
	parent *World
	funcs  FuncScope
	vars   Scope
}

type _Reader interface {
	io.RuneScanner
	io.ByteReader
}

type _ReaderNode struct {
	_Reader
}

type _Writer = io.Writer

type _WriterNode struct {
	_Writer
	lastOutputIsNotLf bool
}

type CanKnowLastOutput interface {
	IsLastOutputLf() bool
}

func (w *_WriterNode) IsLastOutputLf() bool {
	return !w.lastOutputIsNotLf
}

func (w *_WriterNode) Write(p []byte) (nn int, err error) {
	nn, err = w._Writer.Write(p)
	if nn >= 1 {
		w.lastOutputIsNotLf = (p[nn-1] != '\n')
	}
	return nn, err
}

func (w *_WriterNode) WriteByte(c byte) error {
	w.lastOutputIsNotLf = (c != '\n')
	_, err := w._Writer.Write([]byte{c})
	return err
}

func (w *_WriterNode) WriteRune(c rune) (int, error) {
	var buffer [utf8.UTFMax]byte
	size := utf8.EncodeRune(buffer[:], c)
	w.lastOutputIsNotLf = (c != '\n')
	return w._Writer.Write(buffer[:size])
}

func (w *_WriterNode) WriteString(s string) (size int, err error) {
	n, err := io.WriteString(w._Writer, s)
	if n > 0 {
		w.lastOutputIsNotLf = (s[n-1] != '\n')
	}
	return n, err
}

func (w *World) Get(name Symbol) (Node, error) {
	for w != nil {
		if w.vars != nil {
			if value, ok := w.vars.Get(name); ok {
				return value, nil
			}
		}
		w = w.parent
	}
	return Null, MakeError(ErrVariableUnbound, name)
}

func (w *World) GetFunc(name Symbol) (Callable, error) {
	for w != nil {
		if w.funcs != nil {
			if value, ok := w.funcs.Get(name); ok {
				return value, nil
			}
		}
		w = w.parent
	}
	return nil, _UndefinedFunction{name: name}
}

// DefineGlobal implements (defglobal) of ISLisp or (defparameter) of CommonLisp.
func (w *World) DefineGlobal(name Symbol, value Node) {
	w.global.Set(name, value)
}

func (w *World) Set(name Symbol, value Node) error {
	for w != nil {
		if w.vars != nil {
			if _, ok := w.vars.Get(name); ok {
				w.vars.Set(name, value)
				return nil
			}
		}
		w = w.parent
	}
	return ErrVariableUnbound
}

func cmdStandardOutput(ctx context.Context, w *World, list Node) (Node, error) {
	if IsSome(list) {
		return nil, ErrTooManyArguments
	}
	return w.stdout, nil
}

func (w *World) Stdout() io.Writer {
	return w.stdout
}

func (w *World) SetStdout(writer io.Writer) {
	w.stdout = &_WriterNode{_Writer: writer}
}

func cmdErrorOutput(ctx context.Context, w *World, list Node) (Node, error) {
	if IsSome(list) {
		return nil, ErrTooManyArguments
	}
	return w.errout, nil
}

func (w *World) Errout() io.Writer {
	return w.errout
}

func (w *World) SetErrout(writer io.Writer) {
	w.errout = &_WriterNode{_Writer: writer}
}

func cmdStandardInput(ctx context.Context, w *World, list Node) (Node, error) {
	return w.stdin, nil
}
func (w *World) Stdin() _ReaderNode {
	return w.stdin
}

var autoLoadVars = Variables{
	NewSymbol("*err-exist*"):              &ErrorNode{Value: os.ErrExist},
	NewSymbol("*err-not-exist*"):          &ErrorNode{Value: os.ErrNotExist},
	NewSymbol("*err-quit*"):               &ErrorNode{Value: ErrQuit},
	NewSymbol("*err-too-few-arguments*"):  &ErrorNode{Value: ErrTooFewArguments},
	NewSymbol("*err-too-many-arguments*"): &ErrorNode{Value: ErrTooManyArguments},
	NewSymbol("*err-too-short-tokens*"):   &ErrorNode{Value: ErrTooShortTokens},
	NewSymbol("*err-variable-unbound*"):   &ErrorNode{Value: ErrVariableUnbound},
	NewSymbol("<error>"):                  errorClass,
	NewSymbol("most-negative-fixnum"):     Integer(math.MinInt),
	NewSymbol("most-positive-fixnum"):     Integer(math.MaxInt),
	NewSymbol("pi"):                       Float(math.Pi),
	domainErrorClass.name:                 domainErrorClass,
	objectClass.name:                      objectClass,
	builtInClass.name:                     builtInClass,
	standardClass.name:                    standardClass,
}

var autoLoadFunc = Functions{
	// *sort*start*
	NewSymbol("*"):                           &Function{F: funMulti},
	NewSymbol("+"):                           &Function{F: funAdd},
	NewSymbol("-"):                           &Function{F: funSub},
	NewSymbol("/"):                           &Function{F: funDevide},
	NewSymbol("/="):                          &Function{C: 2, F: funNotEqual},
	NewSymbol("<"):                           &Function{F: funLessThan},
	NewSymbol("<="):                          &Function{F: funLessOrEqual},
	NewSymbol("="):                           &Function{F: funEqualOp},
	NewSymbol(">"):                           &Function{F: funGreaterThan},
	NewSymbol(">="):                          &Function{F: funGreaterOrEqual},
	NewSymbol("abort"):                       SpecialF(cmdAbort),
	NewSymbol("and"):                         SpecialF(cmdAnd),
	NewSymbol("append"):                      &Function{F: funAppend},
	NewSymbol("apply"):                       SpecialF(cmdApply),
	NewSymbol("aref"):                        &Function{F: funAref},
	NewSymbol("array-dimensions"):            &Function{C: 1, F: funArrayDimensions},
	NewSymbol("assoc"):                       &Function{C: 2, F: funAssoc},
	NewSymbol("assure"):                      &Function{F: funAssure},
	NewSymbol("atom"):                        &Function{C: 1, F: funAtom},
	NewSymbol("backquote"):                   SpecialF(cmdBackQuote),
	NewSymbol("basic-array*-p"):              &Function{C: 1, F: funGeneralArray},
	NewSymbol("basic-array-p"):               &Function{C: 1, F: funBasicArray},
	NewSymbol("block"):                       SpecialF(cmdBlock),
	NewSymbol("car"):                         &Function{C: 1, F: funGetCar},
	NewSymbol("case"):                        SpecialF(cmdCase),
	NewSymbol("catch"):                       SpecialF(cmdCatch),
	NewSymbol("cdr"):                         &Function{C: 1, F: funGetCdr},
	NewSymbol("ceiling"):                     &Function{C: 1, F: funCeiling},
	NewSymbol("char-index"):                  &Function{Min: 2, Max: 3, F: funRuneIndex},
	NewSymbol("char/="):                      &Function{C: 2, F: funRuneNe},
	NewSymbol("char<"):                       &Function{C: 2, F: funRuneLt},
	NewSymbol("char<="):                      &Function{C: 2, F: funRuneLe},
	NewSymbol("char="):                       &Function{C: 2, F: funRuneEq},
	NewSymbol("char>"):                       &Function{C: 2, F: funRuneGt},
	NewSymbol("char>="):                      &Function{C: 2, F: funRuneGe},
	NewSymbol("characterp"):                  &Function{C: 1, F: funAnyTypep[Rune]},
	NewSymbol("class"):                       SpecialF(cmdClass),
	NewSymbol("class-of"):                    &Function{C: 1, F: funClassOf},
	NewSymbol("close"):                       &Function{C: 1, F: funClose},
	NewSymbol("clrhash"):                     &Function{C: 1, F: funClearHash},
	NewSymbol("cond"):                        SpecialF(cmdCond),
	NewSymbol("cons"):                        &Function{C: 2, F: funCons},
	NewSymbol("consp"):                       &Function{C: 1, F: funAnyTypep[*Cons]},
	NewSymbol("continue-condition"):          SpecialF(cmdContinueCondition),
	NewSymbol("convert"):                     SpecialF(cmdConvert),
	NewSymbol("create"):                      SpecialF(cmdCreate),
	NewSymbol("create-array"):                &Function{F: funCreateArray},
	NewSymbol("create-list"):                 &Function{Min: 2, F: funCreateList},
	NewSymbol("create-string"):               &Function{F: funCreateString},
	NewSymbol("create-string-input-stream"):  &Function{C: 1, F: funCreateStringInputStream},
	NewSymbol("create-string-output-stream"): SpecialF(cmdCreateStringOutputStream),
	NewSymbol("defclass"):                    SpecialF(cmdDefClass),
	NewSymbol("defconstant"):                 SpecialF(cmdDefglobal),
	NewSymbol("defdynamic"):                  SpecialF(cmdDefDynamic),
	NewSymbol("defgeneric"):                  SpecialF(cmdDefGeneric),
	NewSymbol("defglobal"):                   SpecialF(cmdDefglobal),
	NewSymbol("defmacro"):                    SpecialF(cmdDefMacro),
	NewSymbol("defmethod"):                   SpecialF(cmdDefMethod),
	NewSymbol("defun"):                       SpecialF(cmdDefun),
	NewSymbol("domain-error-expected-class"): &Function{C: 1, F: funDomainErrorExpectedClass},
	NewSymbol("domain-error-object"):         &Function{C: 1, F: funDomainErrorObject},
	NewSymbol("dynamic"):                     SpecialF(cmdDynamic),
	NewSymbol("dynamic-let"):                 SpecialF(cmdDynamicLet),
	NewSymbol("elt"):                         &Function{Min: 2, F: funElt},
	NewSymbol("eq"):                          SpecialF(cmdEq),
	NewSymbol("eql"):                         SpecialF(cmdEql),
	NewSymbol("equal"):                       SpecialF(cmdEqual),
	NewSymbol("equalp"):                      &Function{F: funEqualOp},
	NewSymbol("error-output"):                SpecialF(cmdErrorOutput),
	NewSymbol("evenp"):                       &Function{C: 1, F: funEvenp},
	NewSymbol("exit"):                        SpecialF(cmdQuit),
	NewSymbol("file-length"):                 &Function{C: 2, F: funFileLength},
	NewSymbol("flet"):                        SpecialF(cmdFlet),
	NewSymbol("floatp"):                      &Function{C: 1, F: funAnyTypep[Float]},
	NewSymbol("floor"):                       &Function{C: 1, F: funFloor},
	NewSymbol("format"):                      &Function{Min: 2, F: funFormat},
	NewSymbol("format-char"):                 &Function{C: 2, F: funFormatChar},
	NewSymbol("format-float"):                &Function{C: 2, F: funFormatFloat},
	NewSymbol("format-integer"):              &Function{C: 3, F: funFormatInteger},
	NewSymbol("format-object"):               &Function{C: 3, F: funFormatObject},
	NewSymbol("funcall"):                     SpecialF(cmdFunCall),
	NewSymbol("function"):                    SpecialF(cmdFunction),
	NewSymbol("functionp"):                   &Function{C: 1, F: funAnyTypep[FunctionRef]},
	NewSymbol("general-array*-p"):            &Function{C: 1, F: funGeneralArray},
	NewSymbol("generic-function-p"):          &Function{C: 1, F: funGenericFunctionP},
	NewSymbol("gensym"):                      SpecialF(cmdGensym),
	NewSymbol("get-output-stream-string"):    &Function{C: 1, F: funGetOutputStreamString},
	NewSymbol("gethash"):                     &Function{C: 2, F: funGetHash},
	NewSymbol("gmn:dump-session"):            SpecialF(cmdDumpSession),
	NewSymbol("go"):                          SpecialF(cmdGo),
	NewSymbol("hash-table-count"):            &Function{C: 1, F: funHashTableCount},
	NewSymbol("if"):                          SpecialF(cmdIf),
	NewSymbol("ignore-errors"):               SpecialF(cmdIgnoreErrors),
	NewSymbol("instancep"):                   SpecialF(defInstanceP),
	NewSymbol("integerp"):                    &Function{C: 1, F: funAnyTypep[Integer]},
	NewSymbol("labels"):                      SpecialF(cmdLabels),
	NewSymbol("lambda"):                      SpecialF(cmdLambda),
	NewSymbol("lambda-macro"):                SpecialF(cmdLambdaMacro),
	NewSymbol("last"):                        &Function{C: 1, F: funLast},
	NewSymbol("length"):                      &Function{C: 1, F: funLength},
	NewSymbol("let"):                         SpecialF(cmdLet),
	NewSymbol("let*"):                        SpecialF(cmdLetX),
	NewSymbol("list"):                        &Function{F: funList},
	NewSymbol("listp"):                       &Function{C: 1, F: funListp},
	NewSymbol("load"):                        &Function{C: 1, F: funLoad},
	NewSymbol("macroexpand"):                 &Function{C: 1, F: funMacroExpand},
	NewSymbol("make-hash-table"):             SpecialF(cmdMakeHashTable),
	NewSymbol("mapc"):                        &Function{F: funMapC},
	NewSymbol("mapcan"):                      &Function{F: funMapCan},
	NewSymbol("mapcar"):                      &Function{F: funMapCar},
	NewSymbol("mapcon"):                      &Function{F: funMapCon},
	NewSymbol("mapl"):                        &Function{F: funMapL},
	NewSymbol("maplist"):                     &Function{F: funMapList},
	NewSymbol("member"):                      &Function{C: 2, F: funMember},
	NewSymbol("minusp"):                      &Function{C: 1, F: funMinusp},
	NewSymbol("mod"):                         &Function{C: 2, F: funMod},
	NewSymbol("not"):                         &Function{C: 1, F: funNot},
	NewSymbol("nreverse"):                    &Function{C: 1, F: funNReverse},
	NewSymbol("null"):                        &Function{C: 1, F: funNullp},
	NewSymbol("numberp"):                     &Function{C: 1, F: funNumberp},
	NewSymbol("oddp"):                        &Function{C: 1, F: funOddp},
	NewSymbol("open-input-file"):             &Function{C: 1, F: funOpenInputFile},
	NewSymbol("open-output-file"):            &Function{C: 1, F: funOpenOutputFile},
	NewSymbol("or"):                          SpecialF(cmdOr),
	NewSymbol("parse-number"):                &Function{C: 1, F: funParseNumber},
	NewSymbol("plusp"):                       &Function{C: 1, F: funPlusp},
	NewSymbol("probe-file"):                  &Function{C: 1, F: funProbeFile},
	NewSymbol("progn"):                       SpecialF(cmdProgn),
	NewSymbol("psetq"):                       SpecialF(cmdPSetq),
	NewSymbol("quit"):                        SpecialF(cmdQuit),
	NewSymbol("quote"):                       SpecialF(cmdQuote),
	NewSymbol("read"):                        &Function{Max: 3, F: funRead},
	NewSymbol("read-char"):                   &Function{Max: 3, F: funReadChar},
	NewSymbol("read-line"):                   &Function{Max: 3, F: funReadLine},
	NewSymbol("rem"):                         &Function{C: 2, F: funRem},
	NewSymbol("remhash"):                     &Function{C: 2, F: funRemoveHash},
	NewSymbol("rest"):                        &Function{C: 1, F: funGetCdr},
	NewSymbol("return"):                      &Function{C: 1, F: funReturn},
	NewSymbol("return-from"):                 SpecialF(cmdReturnFrom),
	NewSymbol("reverse"):                     &Function{C: 1, F: funReverse},
	NewSymbol("round"):                       &Function{C: 1, F: funRound},
	NewSymbol("set-aref"):                    &Function{Min: 3, F: funSetAref},
	NewSymbol("set-car"):                     &Function{C: 2, F: funSetCar},
	NewSymbol("set-cdr"):                     &Function{C: 2, F: funSetCdr},
	NewSymbol("set-gethash"):                 &Function{C: 3, F: funSetHash},
	NewSymbol("setq"):                        SpecialF(cmdSetq),
	NewSymbol("signal-condition"):            &Function{C: 2, F: funSignalCondition},
	NewSymbol("sqrt"):                        &Function{C: 1, F: funSqrt},
	NewSymbol("standard-input"):              SpecialF(cmdStandardInput),
	NewSymbol("standard-output"):             SpecialF(cmdStandardOutput),
	NewSymbol("string-append"):               &Function{F: funStringAppend},
	NewSymbol("string-index"):                &Function{F: funStringIndex},
	NewSymbol("string/="):                    &Function{C: 2, F: funStringNe},
	NewSymbol("string<"):                     &Function{C: 2, F: funStringLt},
	NewSymbol("string<="):                    &Function{C: 2, F: funStringLe},
	NewSymbol("string="):                     &Function{C: 2, F: funStringEq},
	NewSymbol("string>"):                     &Function{C: 2, F: funStringGt},
	NewSymbol("string>="):                    &Function{C: 2, F: funStringGe},
	NewSymbol("stringp"):                     &Function{C: 1, F: funAnyTypep[String]},
	NewSymbol("subclassp"):                   &Function{C: 2, F: funSubClassP},
	NewSymbol("subseq"):                      &Function{C: 3, F: funSubSeq},
	NewSymbol("symbolp"):                     &Function{C: 1, F: funAnyTypep[Symbol]},
	NewSymbol("tagbody"):                     SpecialF(cmdTagBody),
	NewSymbol("the"):                         &Function{F: funAssure},
	NewSymbol("throw"):                       &Function{C: 2, F: funThrow},
	NewSymbol("trace"):                       SpecialF(cmdTrace),
	NewSymbol("truncate"):                    &Function{C: 1, F: funTruncate},
	NewSymbol("unwind-protect"):              SpecialF(cmdUnwindProtect),
	NewSymbol("vector"):                      &Function{F: funVector},
	NewSymbol("while"):                       SpecialF(cmdWhile),
	NewSymbol("with-handler"):                SpecialF(cmdWithHandler),
	NewSymbol("with-open-input-file"):        SpecialF(cmdWithOpenInputFile),
	NewSymbol("with-open-output-file"):       SpecialF(cmdWithOpenOutputFile),
	NewSymbol("with-standard-input"):         SpecialF(cmdWithStandardInput),
	NewSymbol("zerop"):                       &Function{C: 1, F: funZerop},
	symReportCondition:                       reportCondition,
	undefinedEntityName.Symbol:               undefinedEntityName,
	undefinedEntityNameSpace.Symbol:          undefinedEntityNameSpace,
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
	fname := "embed/" + symbol.String() + ".lsp"

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
}

func New() *World {
	rwvars := &autoLoadVars
	rwfuncs := _RootWorld{}
	w := &World{
		shared: &shared{
			global:  rwvars,
			defun:   rwfuncs,
			dynamic: Variables{},
			stdin:   _ReaderNode{_Reader: bufio.NewReader(os.Stdin)},
			stdout:  &_WriterNode{_Writer: os.Stdout},
			errout:  &_WriterNode{_Writer: os.Stderr},
		},
		vars:  rwvars,
		funcs: rwfuncs,
	}
	return w
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

func (w *World) inject(ctx context.Context, list Node, f func(left, right Node) (Node, error)) (Node, error) {
	result, list, err := w.ShiftAndEvalCar(ctx, list)
	if err != nil {
		return nil, err
	}
	for IsSome(list) {
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

func (w *World) Range(f func(Symbol, Node) bool) {
	marked := map[Symbol]struct{}{}
	for ; w != nil; w = w.parent {
		w.vars.Range(func(key Symbol, val Node) bool {
			if _, ok := marked[key]; !ok {
				if !f(key, val) {
					return false
				}
				marked[key] = struct{}{}
			}
			return true
		})
	}
}

func cmdDumpSession(_ context.Context, w *World, _ Node) (Node, error) {
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
