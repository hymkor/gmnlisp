package gmnlisp

import (
	"context"
	"fmt"
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
