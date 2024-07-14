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

var (
	symVariable = NewSymbol("variable")
	symFunction = NewSymbol("function")
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
	macro     map[Symbol]*_Macro
	handler   Callable
	global    Scope
	defun     FuncScope
	dynamic   Variables
	stdout    *_WriterNode
	errout    *_WriterNode
	stdin     _ReaderNode
	startup   sync.Once
	blockName map[Symbol]struct{}
	catchTag  map[Node]struct{}
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
	return Null, &_UndefinedEntity{name: name, space: symVariable}
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
				w.vars.Set(name, value)
				return nil
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
	w.stdout = &_WriterNode{_Writer: writer}
}

func funErrorOutput(ctx context.Context, w *World) (Node, error) {
	return w.errout, nil
}

func (w *World) Errout() io.Writer {
	return w.errout
}

func (w *World) SetErrout(writer io.Writer) {
	w.errout = &_WriterNode{_Writer: writer}
}

func funStandardInput(ctx context.Context, w *World) (Node, error) {
	return w.stdin, nil
}
func (w *World) Stdin() _ReaderNode {
	return w.stdin
}

var autoLoadVars = Variables{
	NewSymbol("*err-exist*"):              ErrorNode{Value: os.ErrExist},
	NewSymbol("*err-not-exist*"):          ErrorNode{Value: os.ErrNotExist},
	NewSymbol("*err-quit*"):               ErrorNode{Value: ErrQuit},
	NewSymbol("*err-too-few-arguments*"):  ErrorNode{Value: ErrTooFewArguments},
	NewSymbol("*err-too-many-arguments*"): ErrorNode{Value: ErrTooManyArguments},
	NewSymbol("*err-too-short-tokens*"):   ErrorNode{Value: ErrTooShortTokens},
	NewSymbol("<error>"):                  errorClass,
	NewSymbol("most-negative-fixnum"):     Integer(math.MinInt),
	NewSymbol("most-positive-fixnum"):     Integer(math.MaxInt),
	NewSymbol("pi"):                       Float(math.Pi),
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
	NewSymbol("abort"):                       Function0(funAbort),
	NewSymbol("and"):                         SpecialF(cmdAnd),
	NewSymbol("append"):                      &Function{F: funAppend},
	NewSymbol("apply"):                       SpecialF(cmdApply),
	NewSymbol("aref"):                        &Function{F: funAref},
	NewSymbol("arithmetic-error-operands"):   Function1(funArithmeticErrorOperands),
	NewSymbol("arithmetic-error-operation"):  Function1(funArithmeticErrorOperation),
	NewSymbol("array-dimensions"):            Function1(funArrayDimensions),
	NewSymbol("assoc"):                       Function2(Assoc),
	NewSymbol("assure"):                      Function2(funAssure),
	NewSymbol("atom"):                        Function1(funAtom),
	NewSymbol("basic-array*-p"):              Function1(funGeneralArray),
	NewSymbol("basic-array-p"):               Function1(funBasicArray),
	NewSymbol("block"):                       SpecialF(cmdBlock),
	NewSymbol("car"):                         Function1(funGetCar),
	NewSymbol("case"):                        SpecialF(cmdCase),
	NewSymbol("catch"):                       SpecialF(cmdCatch),
	NewSymbol("cdr"):                         Function1(funGetCdr),
	NewSymbol("ceiling"):                     Function1(funCeiling),
	NewSymbol("char-index"):                  &Function{Min: 2, Max: 3, F: funRuneIndex},
	NewSymbol("char/="):                      &Function{C: 2, F: funRuneNe},
	NewSymbol("char<"):                       &Function{C: 2, F: funRuneLt},
	NewSymbol("char<="):                      &Function{C: 2, F: funRuneLe},
	NewSymbol("char="):                       &Function{C: 2, F: funRuneEq},
	NewSymbol("char>"):                       &Function{C: 2, F: funRuneGt},
	NewSymbol("char>="):                      &Function{C: 2, F: funRuneGe},
	NewSymbol("characterp"):                  Function1(funAnyTypep[Rune]),
	NewSymbol("class"):                       SpecialF(cmdClass),
	NewSymbol("class-of"):                    Function1(funClassOf),
	NewSymbol("close"):                       Function1(funClose),
	NewSymbol("clrhash"):                     Function1(funClearHash),
	NewSymbol("cond"):                        SpecialF(cmdCond),
	NewSymbol("cons"):                        Function2(funCons),
	NewSymbol("consp"):                       Function1(funAnyTypep[*Cons]),
	NewSymbol("continue-condition"):          SpecialF(cmdContinueCondition),
	NewSymbol("convert"):                     SpecialF(cmdConvert),
	NewSymbol("create"):                      &Function{F: funCreate},
	NewSymbol("create-array"):                &Function{F: funCreateArray},
	NewSymbol("create-list"):                 &Function{Min: 2, F: funCreateList},
	NewSymbol("create-string"):               &Function{F: funCreateString},
	NewSymbol("create-string-input-stream"):  Function1(funCreateStringInputStream),
	NewSymbol("create-string-output-stream"): Function0(funCreateStringOutputStream),
	NewSymbol("defclass"):                    SpecialF(cmdDefClass),
	NewSymbol("defconstant"):                 SpecialF(cmdDefglobal),
	NewSymbol("defdynamic"):                  SpecialF(cmdDefDynamic),
	NewSymbol("defgeneric"):                  SpecialF(cmdDefGeneric),
	NewSymbol("defglobal"):                   SpecialF(cmdDefglobal),
	NewSymbol("defmacro"):                    SpecialF(cmdDefMacro),
	NewSymbol("defmethod"):                   SpecialF(cmdDefMethod),
	NewSymbol("defun"):                       SpecialF(cmdDefun),
	NewSymbol("domain-error-expected-class"): Function1(funDomainErrorExpectedClass),
	NewSymbol("domain-error-object"):         Function1(funDomainErrorObject),
	NewSymbol("dynamic"):                     SpecialF(cmdDynamic),
	NewSymbol("dynamic-let"):                 SpecialF(cmdDynamicLet),
	NewSymbol("elt"):                         &Function{Min: 2, F: funElt},
	NewSymbol("eq"):                          &Function{Min: 1, F: funEq},
	NewSymbol("eql"):                         &Function{Min: 1, F: funEql},
	NewSymbol("equal"):                       &Function{Min: 1, F: funEqual},
	NewSymbol("equalp"):                      &Function{Min: 1, F: funEqualOp},
	NewSymbol("error-output"):                Function0(funErrorOutput),
	NewSymbol("eval"):                        Function1(funEval),
	NewSymbol("evenp"):                       Function1(funEvenp),
	NewSymbol("exit"):                        Function0(funQuit),
	NewSymbol("expand-defun"):                SpecialF(cmdExpandDefun),
	NewSymbol("file-length"):                 Function2(funFileLength),
	NewSymbol("flet"):                        SpecialF(cmdFlet),
	NewSymbol("floatp"):                      Function1(funAnyTypep[Float]),
	NewSymbol("floor"):                       Function1(funFloor),
	NewSymbol("format"):                      &Function{Min: 2, F: funFormat},
	NewSymbol("format-char"):                 &Function{C: 2, F: funFormatChar},
	NewSymbol("format-float"):                &Function{C: 2, F: funFormatFloat},
	NewSymbol("format-integer"):              &Function{C: 3, F: funFormatInteger},
	NewSymbol("format-object"):               &Function{C: 3, F: funFormatObject},
	NewSymbol("funcall"):                     SpecialF(cmdFunCall),
	NewSymbol("function"):                    SpecialF(cmdFunction),
	NewSymbol("functionp"):                   Function1(funAnyTypep[FunctionRef]),
	NewSymbol("general-array*-p"):            Function1(funGeneralArray),
	NewSymbol("generic-function-p"):          Function1(funGenericFunctionP),
	NewSymbol("gensym"):                      Function0(funGensym),
	NewSymbol("get-output-stream-string"):    Function1(funGetOutputStreamString),
	NewSymbol("gethash"):                     Function2(funGetHash),
	NewSymbol("gmn:dump-session"):            Function0(funDumpSession),
	NewSymbol("go"):                          SpecialF(cmdGo),
	NewSymbol("hash-table-count"):            Function1(funHashTableCount),
	NewSymbol("if"):                          SpecialF(cmdIf),
	NewSymbol("ignore-errors"):               SpecialF(cmdIgnoreErrors),
	NewSymbol("instancep"):                   SpecialF(defInstanceP),
	NewSymbol("integerp"):                    Function1(funAnyTypep[Integer]),
	NewSymbol("labels"):                      SpecialF(cmdLabels),
	NewSymbol("lambda"):                      SpecialF(cmdLambda),
	NewSymbol("lambda-macro"):                SpecialF(cmdLambdaMacro),
	NewSymbol("last"):                        Function1(funLast),
	NewSymbol("length"):                      Function1(funLength),
	NewSymbol("let"):                         SpecialF(cmdLet),
	NewSymbol("let*"):                        SpecialF(cmdLetX),
	NewSymbol("list"):                        &Function{F: funList},
	NewSymbol("listp"):                       Function1(funListp),
	NewSymbol("load"):                        Function1(funLoad),
	NewSymbol("macroexpand"):                 Function1(funMacroExpand),
	NewSymbol("make-hash-table"):             Function0(funMakeHashTable),
	NewSymbol("mapc"):                        &Function{F: funMapC},
	NewSymbol("mapcan"):                      &Function{F: funMapCan},
	NewSymbol("mapcar"):                      &Function{F: funMapCar},
	NewSymbol("mapcon"):                      &Function{F: funMapCon},
	NewSymbol("mapl"):                        &Function{F: funMapL},
	NewSymbol("maplist"):                     &Function{F: funMapList},
	NewSymbol("member"):                      Function2(funMember),
	NewSymbol("minusp"):                      Function1(funMinusp),
	NewSymbol("mod"):                         Function2(funMod),
	NewSymbol("not"):                         Function1(funNot),
	NewSymbol("nreverse"):                    Function1(NReverse),
	NewSymbol("null"):                        Function1(funNullp),
	NewSymbol("numberp"):                     Function1(funNumberp),
	NewSymbol("oddp"):                        Function1(funOddp),
	NewSymbol("open-input-file"):             Function1(funOpenInputFile),
	NewSymbol("open-output-file"):            Function1(funOpenOutputFile),
	NewSymbol("or"):                          SpecialF(cmdOr),
	NewSymbol("parse-number"):                Function1(funParseNumber),
	NewSymbol("plusp"):                       Function1(funPlusp),
	NewSymbol("probe-file"):                  Function1(funProbeFile),
	NewSymbol("progn"):                       SpecialF(cmdProgn),
	NewSymbol("psetq"):                       SpecialF(cmdPSetq),
	NewSymbol("quasiquote"):                  SpecialF(cmdQuasiQuote),
	NewSymbol("quit"):                        Function0(funQuit),
	NewSymbol("quote"):                       SpecialF(cmdQuote),
	NewSymbol("read"):                        &Function{Max: 3, F: funRead},
	NewSymbol("read-char"):                   &Function{Max: 3, F: funReadChar},
	NewSymbol("read-line"):                   &Function{Max: 3, F: funReadLine},
	NewSymbol("rem"):                         Function2(funRem),
	NewSymbol("remhash"):                     Function2(funRemoveHash),
	NewSymbol("rest"):                        Function1(funGetCdr),
	NewSymbol("return"):                      Function1(funReturn),
	NewSymbol("return-from"):                 SpecialF(cmdReturnFrom),
	NewSymbol("reverse"):                     Function1(funReverse),
	NewSymbol("round"):                       Function1(funRound),
	NewSymbol("set-aref"):                    &Function{Min: 3, F: funSetAref},
	NewSymbol("set-car"):                     Function2(funSetCar),
	NewSymbol("set-cdr"):                     Function2(funSetCdr),
	NewSymbol("set-gethash"):                 &Function{C: 3, F: funSetHash},
	NewSymbol("setq"):                        SpecialF(cmdSetq),
	NewSymbol("signal-condition"):            Function2(funSignalCondition),
	NewSymbol("sqrt"):                        Function1(funSqrt),
	NewSymbol("standard-input"):              Function0(funStandardInput),
	NewSymbol("standard-output"):             Function0(funStandardOutput),
	NewSymbol("string-append"):               &Function{F: funStringAppend},
	NewSymbol("string-index"):                &Function{F: funStringIndex},
	NewSymbol("string/="):                    &Function{C: 2, F: funStringNe},
	NewSymbol("string<"):                     &Function{C: 2, F: funStringLt},
	NewSymbol("string<="):                    &Function{C: 2, F: funStringLe},
	NewSymbol("string="):                     &Function{C: 2, F: funStringEq},
	NewSymbol("string>"):                     &Function{C: 2, F: funStringGt},
	NewSymbol("string>="):                    &Function{C: 2, F: funStringGe},
	NewSymbol("stringp"):                     Function1(funAnyTypep[String]),
	NewSymbol("subclassp"):                   &Function{C: 2, F: funSubClassP},
	NewSymbol("subseq"):                      &Function{C: 3, F: funSubSeq},
	NewSymbol("symbolp"):                     Function1(funAnyTypep[Symbol]),
	NewSymbol("tagbody"):                     SpecialF(cmdTagBody),
	NewSymbol("the"):                         Function2(funAssure),
	NewSymbol("throw"):                       Function2(funThrow),
	NewSymbol("trace"):                       SpecialF(cmdTrace),
	NewSymbol("truncate"):                    Function1(funTruncate),
	NewSymbol("undefined-entity-name"):       Function1(funUndefinedEntityName),
	NewSymbol("undefined-entity-namespace"):  Function1(funUndefinedEntityNamespace),
	NewSymbol("unwind-protect"):              SpecialF(cmdUnwindProtect),
	NewSymbol("vector"):                      &Function{F: funVector},
	NewSymbol("while"):                       SpecialF(cmdWhile),
	NewSymbol("with-handler"):                SpecialF(cmdWithHandler),
	NewSymbol("with-open-input-file"):        SpecialF(cmdWithOpenInputFile),
	NewSymbol("with-open-output-file"):       SpecialF(cmdWithOpenOutputFile),
	NewSymbol("with-standard-input"):         SpecialF(cmdWithStandardInput),
	NewSymbol("zerop"):                       Function1(funZerop),
	symReportCondition:                       reportCondition,
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
