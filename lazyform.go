package gmnlisp

import (
	"context"
	"embed"
	"fmt"
	"strings"
)

type lazyForm struct {
	S string
	f Callable
}

func (L *lazyForm) Callable(ctx context.Context, w *World) (Callable, error) {
	if L.f == nil {
		c, err := w.Interpret(ctx, L.S)
		if err != nil {
			return nil, err
		}
		L.f, err = ExpectFunction(ctx, w, c)
		if err != nil {
			L.f = nil
			return nil, fmt.Errorf("lazyForm Call: %w", err)
		}
	}
	return L.f, nil
}

func (L *lazyForm) Call(ctx context.Context, w *World, n Node) (Node, error) {
	f, err := L.Callable(ctx, w)
	if err != nil {
		return nil, err
	}
	return f.Call(ctx, w, n)
}

func (L *lazyForm) FuncId() uintptr {
	return funcToId(L)
}

//go:embed embed/*
var embedLisp embed.FS

type rootFuncTable map[Symbol]Callable

func (rw rootFuncTable) Get(symbol Symbol) (Callable, bool) {
	if value, ok := rw[symbol]; ok {
		return value, true
	}
	if value, ok := autoLoadFunc[symbol]; ok {
		return value, true
	}
	fname := "embed/" + strings.ToLower(symbol.String()) + ".lsp"

	script, err := embedLisp.ReadFile(fname)
	if err == nil {
		value := &lazyForm{S: string(script)}
		autoLoadFunc[symbol] = value
		return value, true
	}
	return nil, false
}

func (rw rootFuncTable) Set(symbol Symbol, value Callable) {
	rw[symbol] = value
}

func (rw rootFuncTable) Range(f func(Symbol, Callable) bool) {
	for key, val := range rw {
		if !f(key, val) {
			break
		}
	}
}
