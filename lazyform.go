package gmnlisp

import (
	"context"
	"fmt"
)

type lazyForm struct {
	S       string
	compile Node
}

func (L *lazyForm) Eval(ctx context.Context, w *World) (Node, error) {
	if L.compile == nil {
		c, err := w.Interpret(ctx, L.S)
		if err != nil {
			return nil, err
		}
		L.compile = c
	}
	return L.compile, nil
}

func (L *lazyForm) Call(ctx context.Context, w *World, n Node) (Node, error) {
	compile, err := L.Eval(ctx, w)
	if err != nil {
		return nil, err
	}
	f, err := ExpectFunction(ctx, w, compile)
	if err != nil {
		return nil, fmt.Errorf("(*LispString) Call: %w", err)
	}
	return f.Call(ctx, w, n)
}

func (L *lazyForm) FuncId() uintptr {
	return funcToId(L)
}
