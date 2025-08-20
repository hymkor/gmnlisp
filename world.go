package gmnlisp

import (
	"bufio"
	"bytes"
	"context"
	"embed"
	"errors"
	"fmt"
	"io"
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
		w.stdout = newOutputFileStream(f, 0)
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
		w.errout = newOutputFileStream(f, 0)
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
