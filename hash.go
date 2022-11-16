package gmnlisp

import (
	"context"
	"errors"
	"fmt"
	"io"
)

type _Hash map[Node]Node

func (h _Hash) Equals(other Node, mode EqlMode) bool {
	return false
}

func (h _Hash) PrintTo(w io.Writer, mode PrintMode) (int, error) {
	n := 0
	dem := '{'
	for key, val := range h {
		_n, err := fmt.Fprintf(w, "%c%#v:%#v", dem, key, val)
		n += _n
		if err != nil {
			return n, err
		}
		dem = ','
	}
	_n, err := w.Write([]byte{'}'})
	n += _n
	return n, err
}

func (h _Hash) Eval(ctx context.Context, w *World) (Node, error) {
	return h, nil
}

func cmdMakeHashTable(ctx context.Context, w *World, _ Node) (Node, error) {
	return make(_Hash), nil
}

var ErrExpectedHash = errors.New("Expected Hash-table")

func canUseHashKey(value Node) bool {
	switch value.(type) {
	case String:
		return true
	case Symbol:
		return true
	case Integer:
		return true
	case Float:
		return true
	case Rune:
		return true
	case Keyword:
		return true
	}
	return false
}

func funGetHash(ctx context.Context, w *World, args []Node) (Node, error) {
	hash, ok := args[1].(_Hash)
	if !ok {
		return nil, ErrExpectedHash
	}
	if !canUseHashKey(args[0]) {
		return nil, ErrNotSupportType
	}
	value, ok := hash[args[0]]
	if !ok {
		return Null, nil
	}
	return value, nil
}

func funSetHash(ctx context.Context, w *World, args []Node) (Node, error) {
	hash, ok := args[2].(_Hash)
	if !ok {
		return nil, ErrExpectedHash
	}
	if !canUseHashKey(args[1]) {
		return nil, ErrNotSupportType
	}
	hash[args[1]] = args[0]
	return args[0], nil
}

func funHashTableCount(ctx context.Context, w *World, args []Node) (Node, error) {
	hash, ok := args[0].(_Hash)
	if !ok {
		return nil, ErrExpectedHash
	}
	return Integer(len(hash)), nil
}

func funRemoveHash(ctx context.Context, w *World, args []Node) (Node, error) {
	hash, ok := args[1].(_Hash)
	if !ok {
		return nil, ErrExpectedHash
	}
	if !canUseHashKey(args[0]) {
		return nil, ErrNotSupportType
	}
	delete(hash, args[0])
	return Null, nil
}

func funClearHash(ctx context.Context, w *World, args []Node) (Node, error) {
	hash, ok := args[0].(_Hash)
	if !ok {
		return nil, ErrExpectedHash
	}
	for key := range hash {
		delete(hash, key)
	}
	return Null, nil
}
